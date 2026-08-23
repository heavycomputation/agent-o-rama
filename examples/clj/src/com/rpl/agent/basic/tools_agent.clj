(ns com.rpl.agent.basic.tools-agent
  "Demonstrates tools integration with native OpenAI chat models.

  Features demonstrated:
  - new-tools-agent: Create specialized agent for tool execution
  - tools/deftool: Define a tool — name, description, JSON schema and
    implementation — in one form
  - OpenAI model with tool calling capabilities
  - Natural language to tool execution workflow"
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; Tool definitions. deftool declares the spec (name, description, JSON
;;; schema) and the implementation together; parameters are bound from the
;;; model's arguments by name.
(tools/deftool calculator
  "Performs basic arithmetic operations on two numbers"
  [operation (schema/enum "The arithmetic operation to perform"
                          ["add" "subtract" "multiply" "divide"])
   a         (schema/number "The first number")
   b         (schema/number "The second number")]
  (str (case operation
         "add"      (+ a b)
         "subtract" (- a b)
         "multiply" (* a b)
         "divide"   (if (zero? b)
                      "Error: Division by zero"
                      (/ a b))
         "Error: Unknown operation")))

(tools/deftool string-processor
  "Performs string manipulation operations"
  [text      (schema/string "The text to process")
   operation (schema/enum "The string operation to perform"
                          ["uppercase" "lowercase" "reverse" "length"])]
  (case operation
    "uppercase" (str/upper-case text)
    "lowercase" (str/lower-case text)
    "reverse"   (str/reverse text)
    "length"    (str (count text))
    "Error: Unknown string operation"))

;;; Agent module demonstrating tools functionality
(aor/defagentmodule ToolsAgentModule
  [topology]

  ;; Declare OpenAI model as an auto-traced agent object
  (openai/declare-model
   topology
   "openai-model"
   {:api-key   (or (System/getenv "OPENAI_API_KEY") "fake-key-for-demo")
    :model     "gpt-5-mini"
    :reasoning {:effort :low}})

  ;; Create tools agent with our tool definitions
  (tools/new-tools-agent
   topology
   "ToolsAgent"
   [calculator string-processor])

  ;; Create a coordinator agent that uses OpenAI with tools
  (->
    topology
    (aor/new-agent "ToolsCoordinator")

    ;; Node that sends natural language prompts to OpenAI and processes tool calls
    (aor/node
     "chat-with-tools"
     nil
     (fn chat-with-tools-fn [agent-node prompts]
       (let [model       (aor/get-agent-object agent-node "openai-model")
             tools-agent (aor/agent-client agent-node "ToolsAgent")
             results     (atom [])]

         (doseq [^String prompt prompts]
           ;; Send prompt to OpenAI model with tools. Frontier models happily
           ;; do trivial arithmetic themselves, so the system prompt steers
           ;; them to demonstrate tool calling instead.
           (let [response   (model/chat
                             model
                             {:messages [(model/system
                                          "When a request involves arithmetic or string manipulation, always use the provided tools rather than answering directly.")
                                         (model/user prompt)]
                              :tools    [calculator string-processor]})
                 tool-calls (:tool-calls response)]

             (if (seq tool-calls)
               ;; Execute tools and get results
               (let [tool-results (aor/agent-invoke tools-agent tool-calls)]
                 (swap! results conj
                   {:prompt       prompt
                    :tool-calls   (count tool-calls)
                    :tool-results tool-results}))
               (swap! results conj
                 {:prompt     prompt
                  :response   (:text response)
                  :tool-calls 0}))))

         (aor/result! agent-node
                      {:prompts-count (count prompts)
                       :results       @results}))))))

(defn -main
  "Run the tools agent example"
  [& _args]
  (with-open [ipc (rtest/create-ipc)]
    (rtest/launch-module! ipc ToolsAgentModule {:tasks 1 :threads 1})
    (let [manager     (aor/agent-manager
                       ipc
                       (rama/get-module-name ToolsAgentModule))
          coordinator (aor/agent-client manager "ToolsCoordinator")
          prompts     ["What is 15 plus 25?"
                       "Calculate 7 times 8"
                       "Divide 100 by 4"
                       "Convert 'Hello World' to uppercase"
                       "Reverse the text 'ReverseMe'"
                       "How many characters are in 'Count Characters'?"]
          result      (aor/agent-invoke coordinator prompts)]

      ;; Create natural language prompts that will trigger tool usage
      (println "Prompts processed:" (:prompts-count result))

      (println "\nDetailed results:")
      (doseq [[idx prompt-result] (map-indexed vector (:results result))]
        (println (format "\n[%d] Prompt: \"%s\"" (inc idx) (:prompt prompt-result)))
        (if (> (:tool-calls prompt-result 0) 0)
          (do
            (println (format "    Tool calls: %d" (:tool-calls prompt-result)))
            ;; Tool results are {:role :tool ...} messages; :content holds
            ;; the tool's output
            (println "    Tool results:"
                     (mapv :content (:tool-results prompt-result))))
          (println "    Direct response:" (:response prompt-result)))))))

(comment
  (-main))
