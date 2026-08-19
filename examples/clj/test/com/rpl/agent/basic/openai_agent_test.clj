(ns com.rpl.agent.basic.openai-agent-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]
   [com.rpl.agent.basic.openai-agent
    :refer [OpenAIAgentModule]]))

(deftest openai-agent-test
  ;; Tests the OpenAIAgent's integration with OpenAI models
  ;; and its ability to process chat requests with configured parameters
  (System/gc)
  (testing "OpenAIAgent with real OpenAI model"
    (if (System/getenv "OPENAI_API_KEY")
      (with-open [ipc (rtest/create-ipc)]
        (rtest/launch-module! ipc OpenAIAgentModule {:tasks 1 :threads 1})

        (let [manager (aor/agent-manager ipc
                                         (rama/get-module-name
                                          OpenAIAgentModule))
              agent   (aor/agent-client manager "OpenAIAgent")]

          (testing "returns response from OpenAI chat model"
            (let [result (aor/agent-invoke agent "What is artificial intelligence?")]
              (is (string? result))
              (is (> (count result) 20)) ; Should get a substantial response
              (is (not (empty? result)))))

          (testing "handles different types of questions"
            (let [result (aor/agent-invoke agent "Explain machine learning briefly")]
              (is (string? result))
              (is (> (count result) 10))))))

      (println "Skipping OpenAIAgent test - OPENAI_API_KEY not set"))))
