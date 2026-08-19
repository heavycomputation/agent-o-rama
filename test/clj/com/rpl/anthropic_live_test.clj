(ns com.rpl.anthropic-live-test
  "Live tests for the native Anthropic adapter. Only run when
  ANTHROPIC_API_KEY is set."
  (:use [clojure.test]
        [com.rpl.test-helpers]
        [com.rpl.rama])
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.anthropic :as anthropic]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama.test :as rtest]))

(def MODEL (or (System/getenv "ANTHROPIC_TEST_MODEL") "claude-haiku-4-5-20251001"))

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

(deftest anthropic-thinking-tools-loop-test
  (when (some? (System/getenv "ANTHROPIC_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (anthropic/declare-model topology
                                   "claude"
                                   {:model      MODEL
                                    :max-tokens 4096
                                    :thinking   {:budget-tokens 1024}})
          (tools/new-tools-agent topology "tools" TOOLS)
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             "chat"
             (fn [agent-node {:keys [messages thought?] :as state}]
               (let [model (aor/get-agent-object agent-node "claude")
                     tools (aor/agent-client agent-node "tools")
                     {:keys [message tool-calls text]}
                     (m/chat model {:messages messages :tools TOOLS})
                     thought? (or thought?
                                  (boolean
                                   (some #(= :reasoning (:type %))
                                         (:content message))))]
                 (if (seq tool-calls)
                   (let [tool-results (aor/agent-invoke tools tool-calls)]
                     (aor/emit! agent-node "chat"
                                {:messages (into (conj messages message)
                                                 tool-results)
                                 :thought? thought?}))
                   (aor/result! agent-node
                                {:text text :thought? thought?}))))))))
       (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
       (bind module-name (get-module-name module))
       (bind agent-manager (aor/agent-manager ipc module-name))
       (bind foo (aor/agent-client agent-manager "foo"))

       (bind res
         (aor/agent-invoke
          foo
          {:messages [(m/user "Use the divide tool to compute 123 divided by 37, then state the numeric result.")]}))
       (is (str/includes? (:text res) "3.32"))
       ;; extended thinking blocks flowed through the tool loop
       (is (true? (:thought? res)))
      ))))

(deftest anthropic-streaming-test
  (when (some? (System/getenv "ANTHROPIC_API_KEY"))
    (with-open [ipc (rtest/create-ipc)]
      (letlocals
       (bind module
         (aor/agentmodule
          [topology]
          (anthropic/declare-model topology
                                   "claude"
                                   {:model      MODEL
                                    :max-tokens 1024
                                    :stream?    true})
          (->
            topology
            (aor/new-agent "foo")
            (aor/node
             "chat"
             nil
             (fn [agent-node ^String prompt]
               (let [model (aor/get-agent-object agent-node "claude")]
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
