(ns com.rpl.openai-live-test
  "Live tests for the native OpenAI Responses adapter. Only run when
  OPENAI_API_KEY is set. Uses a small model with low reasoning effort to
  keep cost/latency down."
  (:use [clojure.test]
        [com.rpl.test-helpers]
        [com.rpl.rama])
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama.test :as rtest]))

(def MODEL "gpt-5-mini")

(def TOOLS
  [(tools/tool
    {:name        "divide"
     :description "Performs division: numerator ÷ denominator. Returns a floating-point result. Denominator must be nonzero."
     :schema      (schema/object
                   {:required ["numerator" "denominator"]}
                   {"numerator"   (schema/number
                                   "The number to be divided (top of the fraction)")
                    "denominator" (schema/number
                                   "The number to divide by; must not be zero")})}
    (fn [args]
      (double
       (/ (get args "numerator")
          (get args "denominator")))))])

(deftest openai-reasoning-tools-loop-test
  (when (some? (System/getenv "OPENAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (openai/declare-model topology
                                "openai"
                                {:model     MODEL
                                 :reasoning {:effort :low}})
          (tools/new-tools-agent topology "tools" TOOLS)
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             "chat"
             (fn [agent-node {:keys [messages reasoned?] :as state}]
               (let [model (aor/get-agent-object agent-node "openai")
                     tools (aor/agent-client agent-node "tools")
                     {:keys [message tool-calls text usage]}
                     (m/chat model {:messages messages :tools TOOLS})
                     reasoned? (or reasoned?
                                   (pos? (or (:reasoning-tokens usage) 0)))]
                 (if (seq tool-calls)
                   (let [tool-results (aor/agent-invoke tools tool-calls)]
                     (aor/emit! agent-node "chat"
                                {:messages  (into (conj messages message)
                                                  tool-results)
                                 :reasoned? reasoned?}))
                   (aor/result! agent-node
                                {:text text :reasoned? reasoned?}))))))))
       (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
       (bind module-name (get-module-name module))
       (bind agent-manager (aor/agent-manager ipc module-name))
       (bind foo (aor/agent-client agent-manager "foo"))

       (bind res
         (aor/agent-invoke
          foo
          {:messages [(m/user "Use the divide tool to compute 123 divided by 37, then state the numeric result.")]}))
       ;; 123/37 = 3.3243...
       (is (str/includes? (:text res) "3.32"))
       ;; reasoning happened alongside tool calling — the lc4j blocker
       (is (true? (:reasoned? res)))
      ))))

(deftest openai-streaming-test
  (when (some? (System/getenv "OPENAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (openai/declare-model topology
                                "openai"
                                {:model     MODEL
                                 :reasoning {:effort :low}
                                 :stream?   true})
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             nil
             (fn [agent-node ^String prompt]
               (let [model (aor/get-agent-object agent-node "openai")]
                 (aor/result! agent-node
                              (:text (m/chat model prompt)))))))))
       (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
       (bind module-name (get-module-name module))
       (bind agent-manager (aor/agent-manager ipc module-name))
       (bind foo (aor/agent-client agent-manager "foo"))

       (bind inv (aor/agent-initiate foo "Reply with a one-sentence fun fact about Clojure."))
       (bind done (promise))
       (aor/agent-stream
        foo
        inv
        "chat"
        (fn [all-chunks _new-chunks _reset? complete?]
          (when complete?
            (deliver done all-chunks))))
       (bind res (aor/agent-result foo inv))
       (bind chunks (deref done 60000 :timeout))
       (is (string? res))
       (is (seq res))
       (is (vector? chunks))
       (is (< 1 (count chunks)))
       ;; concatenated text deltas equal the final text
       (is (= res (apply str chunks)))
      ))))

(deftest openai-structured-output-test
  (when (some? (System/getenv "OPENAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (openai/declare-model topology
                                "openai"
                                {:model     MODEL
                                 :reasoning {:effort :low}})
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             nil
             (fn [agent-node ^String prompt]
               (let [model (aor/get-agent-object agent-node "openai")
                     res   (m/chat
                            model
                            {:messages      [(m/user prompt)]
                             :output-schema {:name   "math"
                                             :schema (schema/strict-object
                                                      {"result"      (schema/number
                                                                      "The numeric result")
                                                       "explanation" (schema/string
                                                                      "One-sentence explanation")})}})]
                 (aor/result! agent-node (:parsed res))))))))
       (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
       (bind module-name (get-module-name module))
       (bind agent-manager (aor/agent-manager ipc module-name))
       (bind foo (aor/agent-client agent-manager "foo"))

       (bind parsed (aor/agent-invoke foo "What is 5 times 5?"))
       (is (map? parsed))
       (is (== 25 (get parsed "result")))
       (is (string? (get parsed "explanation")))
      ))))
