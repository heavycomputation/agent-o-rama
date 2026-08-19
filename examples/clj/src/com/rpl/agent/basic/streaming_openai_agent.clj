(ns com.rpl.agent.basic.streaming-openai-agent
  "Demonstrates native OpenAI streaming chat model integration with
  agent-o-rama.

  Features demonstrated:
  - OpenAI streaming model declaration as agent object (:stream? true)
  - Automatic streaming chunk emission from OpenAI streaming responses
  - agent-stream subscription to receive streaming tokens in real-time
  - Single-node streaming chat completion"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; Agent module demonstrating streaming native OpenAI integration
(aor/defagentmodule StreamingOpenAIAgentModule
  [topology]

  ;; Declare OpenAI model as an auto-traced agent object that streams by
  ;; default
  (openai/declare-model
   topology
   "openai-streaming-model"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"
    :reasoning   {:effort :low}
    :stream?     true})

  (-> (aor/new-agent topology "StreamingOpenAIAgent")

      ;; Single node that sends user message to streaming OpenAI model
      (aor/node
       "streaming-chat"
       nil
       (fn [agent-node ^String user-message]
         (let [model    (aor/get-agent-object agent-node "openai-streaming-model")
               messages [(model/system "You are a helpful assistant.")
                         (model/user user-message)]

               ;; Send chat request to streaming OpenAI model
               ;; Streaming chunks are automatically emitted by agent-o-rama
               response (model/chat model {:messages messages})]

           (aor/result! agent-node (:text response)))))))

(defn -main
  "Run the streaming OpenAI agent example"
  [& _args]
  (if (System/getenv "OPENAI_API_KEY")
    (with-open [ipc (rtest/create-ipc)]
      (rtest/launch-module! ipc StreamingOpenAIAgentModule {:tasks 1 :threads 1})

      (let [module-name (rama/get-module-name StreamingOpenAIAgentModule)
            manager     (aor/agent-manager ipc module-name)
            agent       (aor/agent-client manager "StreamingOpenAIAgent")]

        (println "Streaming OpenAI Agent Example:")
        (println "Asking OpenAI a question with real-time streaming...\n")

        ;; Start async agent execution
        (let [invoke (aor/agent-initiate agent "Explain what machine learning is in simple terms")
              streaming-chunks (atom [])]

          (println "User: Explain what machine learning is in simple terms")
          (println "\nAssistant (streaming): ")

          ;; Subscribe to streaming chunks as they arrive from OpenAI
          (aor/agent-stream
           agent
           invoke
           "streaming-chat"
           (fn [all-chunks new-chunks reset? complete?]
             (doseq [chunk new-chunks]
               ;; Print each streaming chunk as it arrives
               (print chunk)
               (flush)
               (swap! streaming-chunks conj chunk))))

          ;; Wait for final complete response
          (let [final-result (aor/agent-result agent invoke)]
            (println "\n\nFinal complete response:")
            (println final-result)
            (println "\nStreaming chunks received:" (count @streaming-chunks)))

          (println "\nNotice how:")
          (println "- OpenAI streaming model automatically emits chunks")
          (println "- agent-stream receives tokens in real-time as they're generated")
          (println "- Final result contains the complete response")
          (println "- No manual stream-chunk! calls needed"))))

    (do
      (println "Streaming OpenAI Agent Example:")
      (println "OPENAI_API_KEY environment variable not set.")
      (println "Please set your OpenAI API key to run this example:")
      (println "  export OPENAI_API_KEY=your-api-key-here"))))

(comment
  (-main))
