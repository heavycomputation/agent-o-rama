(ns com.rpl.agent.basic.openai-agent
  "Demonstrates native OpenAI chat model integration with agent-o-rama.

  Features demonstrated:
  - OpenAI model declaration as agent object with openai/declare-model
  - Message handling with model/system and model/user
  - Chat request with a token limit
  - Simple single-node chat completion"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; Agent module demonstrating native OpenAI integration
(aor/defagentmodule OpenAIAgentModule
  [topology]

  ;; Declare OpenAI chat model as an auto-traced agent object
  (openai/declare-model
   topology
   "openai-model"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"
    :reasoning   {:effort :low}})

  (-> (aor/new-agent topology "OpenAIAgent")

      ;; Single node that sends user message to OpenAI and returns response
      (aor/node
       "chat"
       nil
       (fn [agent-node ^String user-message]
         (let [model    (aor/get-agent-object agent-node "openai-model")
               messages [(model/system "You are a helpful assistant.")
                         (model/user user-message)]

               ;; Send chat request to OpenAI
               response (model/chat
                         model
                         {:messages          messages
                          :max-output-tokens 1000})]

           (aor/result! agent-node (:text response)))))))

(defn -main
  "Run the OpenAI agent example"
  [& _args]
  (if (System/getenv "OPENAI_API_KEY")
    (with-open [ipc (rtest/create-ipc)]
      (rtest/launch-module! ipc OpenAIAgentModule {:tasks 1 :threads 1})

      (let [module-name (rama/get-module-name OpenAIAgentModule)
            manager     (aor/agent-manager ipc module-name)
            agent       (aor/agent-client manager "OpenAIAgent")]

        (println "OpenAI Agent Example:")
        (println "Sending message to OpenAI...\n")

        (let [result (aor/agent-invoke agent "What is agent-o-rama?")]
          (println "User: What is agent-o-rama?")
          (println "\nAssistant:" result))

        (println "\nNotice how:")
        (println "- OpenAI model is declared as an agent object")
        (println "- Single node handles the complete chat interaction")
        (println "- Reasoning effort and token limits are customizable")))

    (do
      (println "OpenAI Agent Example:")
      (println "OPENAI_API_KEY environment variable not set.")
      (println "Please set your OpenAI API key to run this example:")
      (println "  export OPENAI_API_KEY=your-api-key-here"))))
