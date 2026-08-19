(ns com.rpl.agent.todo
  "An agent to manage TODO items.
  Uses long term memory for accumulating a profile and the TODO items."
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.store :as store]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama :as rama]
   [com.rpl.rama.path :as path]
   [com.rpl.rama.test :as rtest]
   [jsonista.core :as j])
  (:import
   [com.rpl.agentorama
    AgentComplete]))

(defn under->dash [s]
  (str/replace s \_  \-))

(defn dash->under [s]
  (str/replace s \-  \_))

(def MAPPER (j/object-mapper
             {:decode-key-fn (comp keyword under->dash)
              :encode-key-fn (comp dash->under str)}))

;; Chatbot instruction for choosing what to update and what tools to call
(def MODEL-SYSTEM-MESSAGE
  "You are a helpful chatbot.

You are designed to be a companion to a user, helping them keep track of their
ToDo list.

You have a long term memory which keeps track of three things:
1. The user's profile (general information about them)
2. The user's ToDo list
3. General instructions for updating the ToDo list

Here is the current User Profile (may be empty if no information has been
collected yet):
<user_profile>
%s
</user_profile>

Here is the current ToDo List (may be empty if no tasks have been added yet):
<todos>
%s
</todos>

Here are the current user-specified preferences for updating the ToDo list (may
be empty if no preferences have been specified yet):
<instructions>
%s
</instructions>

Here are your instructions for reasoning about the user's messages:

1. Reason carefully about the user's messages as presented below.

2. Decide whether any of the your long-term memory should be updated:
- If personal information was provided about the user, update the user's profile
  by calling UpdateMemory tool with type `profile`
- If tasks are mentioned, update the ToDo list by calling UpdateMemory tool with
  type `todo`
- If the user has specified preferences for how to update the ToDo list, update
  the instructions by calling UpdateMemory tool with type `instructions`

3. Tell the user that you have updated your memory, if appropriate:
- Do not tell the user you have updated the user's profile
- Tell the user them when you update the todo list
- Do not tell the user that you have updated instructions

4. Err on the side of updating the todo list. No need to ask for explicit
permission.

5. Respond naturally to user user after a tool call was made to save memories,
or if no tool call was made.")

(def UPDATE-PROFILE
  "Reflect on the following interaction.

Extract a profile of the user.

Create the expected response format based solely on the information available in
the chat. If you don't have information to put in specific fields, or you want
to leave them with their current values, then repeat their current values (use
empty strings or empty lists for fields with no information).

<current_profile>
%s
</current_profile>")

(def UPDATE-TODOS
  "Reflect on the following interaction.

Extract todos for the user.

Create the expected response format based solely on the information available in
the chat. If you don't have information to put in specific fields, leave them
blank.

Do not remove todo items unless explicitly requested to do so. Combine existing
todo items with any new todo items.

<current_todos>
%s
</current_todos>")

(def CREATE-INSTRUCTIONS
  "Reflect on the following interaction.

Based on this interaction, update your instructions for how to update ToDo
list items. Use any feedback from the user to update how they like to have
items added, etc.

Your current instructions are:

<current_instructions>
%s
</current_instructions>")

(def ^:private Profile
  (schema/strict-object
   {:description "The profile of a user."}
   {"name"        (schema/string "The user's name")
    "job"         (schema/string "The user's job")
    "connections" (schema/array
                   "Personal connection of the user, such as family members, friends, or coworkers"
                   (schema/string "A personal connection"))
    "interests"   (schema/array
                   "Interests that the user has"
                   (schema/string "An interest that the user has"))}))

(def ^:private ToDoFields
  {"task"      (schema/string "The task to be completed.")
   "deadline"  (schema/string
                "When the task needs to be completed by (if applicable)")
   "solutions" (schema/array
                "List of specific, actionable solutions (e.g., specific ideas, service providers, or concrete options relevant to completing the task)",
                (schema/string "A specific, actionable solution"))
   "status"    (schema/enum
                "Current status of the task"
                ["not started" "in progress" "done" "archived"])})

(def ^:private ToDo
  (schema/object
   {:description "A ToDo item"
    :required    ["task"]}
   ToDoFields))

(def ^:private Instruction
  (schema/strict-object
   {:description "Instruction"}
   {"instructions" (schema/string "instructions")}))

(defn create-todo-tool
  [agent-node {:keys [user-id]} todo]
  (let [store (aor/get-store agent-node "$$todos")
        uuid  (str (random-uuid))]
    (store/pstate-transform!
     [(path/keypath user-id)
      (path/keypath uuid)
      (path/termval todo)]
     store
     user-id)
    "created"))

(defn update-todo-tool
  [agent-node {:keys [user-id]} arguments]
  (let [store (aor/get-store agent-node "$$todos")
        uuid  (arguments "uuid")
        todo  (arguments "todo")]
    (store/pstate-transform!
     [(path/keypath user-id)
      (path/keypath uuid)
      (path/term #(merge % todo))]
     store
     user-id)
    "updated"))

(defn delete-todo-tool
  [agent-node {:keys [user-id]} arguments]
  (let [store (aor/get-store agent-node "$$todos")
        uuid  (arguments "uuid")]
    (store/pstate-transform!
     [(path/keypath user-id)
      (path/keypath uuid)
      path/NONE]
     store
     user-id)
    "deleted"))

(def TODO-TOOLS
  [(tools/tool
    {:name        "CreateToDo"
     :description "Creates a todo using info from chat messages"
     :schema      ToDo}
    create-todo-tool
    {:include-context? true})
   (tools/tool
    {:name        "UpdateToDo"
     :description "Updates an existing todo using from chat messages"
     :schema      (schema/object
                   {:description "Instruction to update an existing ToDo item"}
                   {"uuid" (schema/string
                            "The uuid identifying the ToDo item to update")
                    "todo" ToDo})}
    update-todo-tool
    {:include-context? true})
   (tools/tool
    {:name        "DeleteToDo"
     :description "Deletes an existing todo identified by its uuid"
     :schema      (schema/object
                   {"uuid" (schema/string
                            "The uuid identifying the ToDo item to delete")})}
    delete-todo-tool
    {:include-context? true})])

(defn update-profile
  [agent-node messages {:keys [user-id]}]
  (let [chat-model    (aor/get-agent-object agent-node "openai-non-streaming")
        store         (aor/get-store agent-node "$$profiles")
        profile       (store/get store user-id)
        system-msg    (format UPDATE-PROFILE profile)
        chat-messages (into
                       [(model/system system-msg)]
                       messages)
        response      (model/chat
                       chat-model
                       {:messages      chat-messages
                        :output-schema {:name   "Profile"
                                        :schema Profile}})
        new-profile   (:parsed response)]
    (store/update! store user-id #(merge % new-profile)))
  "updated")

(defn update-todo
  [agent-node messages {:keys [user-id] :as config}]
  (let [chat-model    (aor/get-agent-object agent-node "openai-non-streaming")
        todo-tools    (aor/agent-client agent-node "todo-tools")
        store         (aor/get-store agent-node "$$todos")
        todos         (into
                       {}
                       (store/pstate-select
                        [(path/keypath user-id) path/ALL]
                        store
                        user-id))
        system-msg    (format
                       UPDATE-TODOS
                       (j/write-value-as-string todos MAPPER))
        chat-messages (->
                       [(model/system system-msg)]
                       (into messages)
                       #_(conj
                          (model/user
                           "Please update the ToDos based on the conversation")))
        {:keys [tool-calls]} (model/chat
                              chat-model
                              {:messages chat-messages
                               :tools    TODO-TOOLS})]
    (when (seq tool-calls)
      (aor/agent-invoke todo-tools tool-calls config))
    "updated"))


(defn update-instruction
  [agent-node messages {:keys [user-id]}]
  (let [chat-model      (aor/get-agent-object agent-node "openai-non-streaming")
        store           (aor/get-store agent-node "$$instructions")
        instruction     (store/get store user-id)
        system-msg      (format
                         CREATE-INSTRUCTIONS
                         instruction)
        chat-messages   (->
                         [(model/system system-msg)]
                         (into messages)
                         (conj
                          (model/user
                           "Please update the instructions based on the conversation")))
        response        (model/chat
                         chat-model
                         {:messages      chat-messages
                          :output-schema {:name   "Instruction"
                                          :schema Instruction}})
        new-instruction (:parsed response)]

    (store/put! store user-id new-instruction)
    "updated"))

(defn update-tool
  [agent-node config arguments]
  (let [update-type (get arguments "update_type")
        messages    (:messages config)]
    (case update-type
      "profile"      (update-profile agent-node messages config)
      "todo"         (update-todo agent-node messages config)
      "instructions" (update-instruction agent-node messages config))))

(def TOOLS
  [(tools/tool
    {:name        "UpdateMemory"
     :description "Updates profile, todo or instruction memory with info from chat messages"
     :schema      (schema/object
                   {:description "Updates persistent memory for info from chat messages"
                    :required    ["update_type"]}
                   {"update_type" (schema/enum
                                   ["profile" "todo" "instructions"])})}
    update-tool
    {:include-context? true})])

(aor/defagentmodule TodoModule
  [topology]

  (openai/declare-model
   topology
   "openai"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"
    :stream?     true})

  (openai/declare-model
   topology
   "openai-non-streaming"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"})

  (aor/declare-document-store
   topology
   "$$profiles"
   Long
   "name" String
   "job" String
   "connections" java.util.List
   "interests" java.util.List)

  (aor/declare-pstate-store
   topology
   "$$todos"
   {Long (rama/map-schema String java.util.Map {:subindex? true})})

  (aor/declare-key-value-store topology "$$instructions" Long Object)

  (->
   topology
   (aor/new-agent "ToDoAgent")

   (aor/node
    "maestro"
    "maestro"
    (fn maestro-node [agent-node messages {:keys [user-id] :as config}]
      (let [chat-model         (aor/get-agent-object
                                agent-node
                                "openai-non-streaming")
            tools              (aor/agent-client agent-node "tools")
            profiles-store     (aor/get-store agent-node "$$profiles")
            todos-store        (aor/get-store agent-node "$$todos")
            instructions-store (aor/get-store agent-node "$$instructions")
            profile            (store/get profiles-store user-id)
            todos              (into
                                {}
                                (store/pstate-select
                                 [(path/keypath user-id) path/ALL]
                                 todos-store
                                 user-id))
            instructions       (store/get instructions-store user-id)
            system-msg         (format
                                MODEL-SYSTEM-MESSAGE
                                profile
                                (j/write-value-as-string todos MAPPER)
                                instructions)
            chat-messages      (into
                                [(model/system system-msg)]
                                messages)
            {:keys [message tool-calls]}
            (model/chat
             chat-model
             {:messages chat-messages
              :tools    TOOLS})
            next-messages      (conj messages message)]
        (if (seq tool-calls)
          (let [tool-results  (aor/agent-invoke
                               tools
                               tool-calls
                               (assoc config :messages messages))
                next-messages (into next-messages tool-results)]
            (aor/emit! agent-node
                       "maestro"
                       next-messages
                       config))
          (aor/result! agent-node {:messages next-messages}))))))

  (tools/new-tools-agent topology "tools" TOOLS)
  (tools/new-tools-agent topology "todo-tools" TODO-TOOLS))

(def inputs
  ["My name is Lance. I live in SF with my wife. I have a 1 year old daughter."
   "My wife asked me to book swim lessons for the baby."
   "When creating or updating ToDo items, include specific local businesses / vendors."
   "I need to fix the jammed electric Yale lock on the door."
   "For the swim lessons, I need to get that done by end of November."
   "Need to call back City Toyota to schedule car service."
   "I have 30 minutes, what tasks can I get done?"
   "Yes, give me some options to call for swim lessons."
   ])

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              _   (aor/start-ui ipc)]
    (rtest/launch-module! ipc TodoModule {:tasks 4 :threads 2})
    (let [module-name   (rama/get-module-name TodoModule)
          agent-manager (aor/agent-manager ipc module-name)
          user-id       0]
      (with-open [agent (aor/agent-client agent-manager "ToDoAgent")]
        (try
          (loop [inputs inputs]
            (when inputs
              (let [agent-invoke (aor/agent-initiate
                                  agent
                                  [(model/user (first inputs))]
                                  {:user-id user-id})
                    step         (aor/agent-next-step agent agent-invoke)
                    result       (:result step)]
                (assert (instance? AgentComplete step))
                (doseq [msg (:messages result)]
                  (println msg))
                (recur (next inputs)))))
          (catch Exception e
            (prn :exeception e))))
      (let [profile-pstate (rama/foreign-pstate ipc module-name "$$profiles")]
        (println
         :profile
         (rama/foreign-select-one (path/keypath user-id) profile-pstate))
        (assert
         (rama/foreign-select-one (path/keypath user-id) profile-pstate)
         "Has a profile")))))
