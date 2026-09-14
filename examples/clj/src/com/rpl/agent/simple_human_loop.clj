;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent.simple-human-loop
  (:use [com.rpl.rama]
        [com.rpl.rama.path])
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.rama.test :as rtest])
  (:import
   [com.rpl.agentorama
    HumanInputRequest]))

(defn human-helpful?
  "Ask user if the response was helpful and loop until valid y/n answer."
  [agent-node response]
  (loop [res (aor/get-human-input
              agent-node
              (str "AI Response: "
                   response
                   "\n\nWas this response helpful? (y/n): "))]
    (cond (= res "y") true
          (= res "n") false
          :else (recur (aor/get-human-input
                        agent-node
                        "Please answer 'y' or 'n'.")))))

(aor/defagentmodule SimpleHumanLoopModule
  [topology]
  (openai/declare-model
   topology
   "openai"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"
    :stream?     true})
  (->
    topology
    (aor/new-agent "simple-chat")
    (aor/node
     "chat"
     nil
     (fn [agent-node user-message]
       (let [openai   (aor/get-agent-object agent-node "openai")
             response (:text (model/chat openai [(model/user user-message)]))
             helpful? (human-helpful? agent-node response)]
         (aor/result! agent-node
                      {:response response
                       :helpful  helpful?}))))))

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              ui  (aor/start-ui ipc)]
    (rtest/launch-module! ipc SimpleHumanLoopModule {:tasks 4 :threads 2})
    (let [module-name   (get-module-name SimpleHumanLoopModule)
          agent-manager (aor/agent-manager ipc module-name)
          chat-agent    (aor/agent-client agent-manager "simple-chat")
          _ (print "Enter your message: ")
          _ (flush)
          user-message  (read-line)
          inv           (aor/agent-initiate chat-agent user-message)]
      (println)
      (println user-message)
      (loop [step (aor/agent-next-step chat-agent inv)]
        (if (instance? HumanInputRequest step)
          (do
            (println (:prompt step))
            (print ">> ")
            (flush)
            (aor/provide-human-input chat-agent step (read-line))
            (println)
            (recur (aor/agent-next-step chat-agent inv)))
          (do
            (println "Final result:")
            (println (:result step))))))))

(comment
  (run-agent))
