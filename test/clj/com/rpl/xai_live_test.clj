(ns com.rpl.xai-live-test
  "Live tests for the native xAI adapter. Only run when XAI_API_KEY is set."
  (:use [clojure.test]
        [com.rpl.test-helpers]
        [com.rpl.rama])
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.xai :as xai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama.test :as rtest]))

(def MODEL (or (System/getenv "XAI_TEST_MODEL") "grok-4.3"))

(def TOOLS
  [(tools/tool
    {:name        "divide"
     :description "Performs division: numerator ÷ denominator. Returns a floating-point result."
     :schema      (schema/object
                   {:required ["numerator" "denominator"]}
                   {"numerator"   (schema/number "The number to be divided")
                    "denominator" (schema/number "The number to divide by")})}
    (fn [args]
      (double
       (/ (get args "numerator")
          (get args "denominator")))))])

(deftest xai-tools-loop-test
  (when (some? (System/getenv "XAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (xai/declare-model topology "grok" {:model MODEL})
          (tools/new-tools-agent topology "tools" TOOLS)
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             "chat"
             (fn [agent-node messages]
               (let [model (aor/get-agent-object agent-node "grok")
                     tools (aor/agent-client agent-node "tools")
                     {:keys [message tool-calls text]}
                     (m/chat model {:messages messages :tools TOOLS})]
                 (if (seq tool-calls)
                   (let [tool-results (aor/agent-invoke tools tool-calls)]
                     (aor/emit! agent-node "chat"
                                (into (conj messages message) tool-results)))
                   (aor/result! agent-node text))))))))
       (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
       (bind module-name (get-module-name module))
       (bind agent-manager (aor/agent-manager ipc module-name))
       (bind foo (aor/agent-client agent-manager "foo"))

       (bind res
         (aor/agent-invoke
          foo
          [(m/user "Use the divide tool to compute 123 divided by 37, then state the numeric result.")]))
       (is (str/includes? res "3.32"))
      ))))

(deftest xai-streaming-test
  (when (some? (System/getenv "XAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (xai/declare-model topology "grok" {:model MODEL :stream? true})
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             nil
             (fn [agent-node ^String prompt]
               (let [model (aor/get-agent-object agent-node "grok")]
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
       (is (= res (apply str chunks)))
      ))))

(deftest xai-structured-output-test
  (when (some? (System/getenv "XAI_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (xai/declare-model topology "grok" {:model MODEL})
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             nil
             (fn [agent-node ^String prompt]
               (let [model (aor/get-agent-object agent-node "grok")
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
