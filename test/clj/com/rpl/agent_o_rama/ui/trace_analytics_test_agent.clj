;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent-o-rama.ui.trace-analytics-test-agent
  "Test agent module for trace analytics E2E tests.

  Provides a configurable agent that can run in different modes to generate
  various types of traces for UI testing."
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.store :as store]
   [com.rpl.agent-o-rama.tools :as tools]))

;;; Tool for :tool-call mode
(defn simple-calculator-tool
  "Simple calculator for testing tool calls"
  [args]
  (let [operation (args "operation")
        a         (args "a")
        b         (args "b")]
    (str
     (case operation
       "add" (+ a b)
       "multiply" (* a b)
       "unknown operation"))))

(def CALCULATOR-TOOL
  (tools/tool
   {:name        "calculator"
    :description "Performs arithmetic operations"
    :schema      (schema/object
                  {:description "Calculator parameters"
                   :required    ["operation" "a" "b"]}
                  {"operation" (schema/enum "The operation"
                                            ["add" "multiply"])
                   "a"         (schema/number "First number")
                   "b"         (schema/number "Second number")})}
   simple-calculator-tool))

(defn test-agent
  [agent-node {:strs [mode input]}]
  (let [mode (if (string? mode) (keyword mode) mode)]
    (case mode
      :basic
      ;; No nested operations, just return result
      (aor/result!
       agent-node
       {"mode" "basic" "result" (str "Processed: " (or input "test"))})

      :sub-agent
      ;; Call sub-agent
      (let [input     (or input "test")
            sub-agent (aor/agent-client agent-node "BasicSubAgent")
            result    (aor/agent-invoke sub-agent input)]
        (aor/result!
         agent-node
         {"mode" "sub-agent" "sub-result" result}))

      :chat
      ;; Use chat model
      (let [input    (or input "hello")
            m        (aor/get-agent-object agent-node "openai-model")
            messages [(model/system "You are helpful.")
                      (model/user (str input))]
            response (model/chat m {:messages messages})]
        (aor/result!
         agent-node
         {"mode"     "chat"
          "response" (:text response)
          "messages" (conj messages (:message response))}))

      :tool-call
      ;; Use chat model with tools
      (let [m              (aor/get-agent-object
                            agent-node
                            "openai-model")
            tools-agent    (aor/agent-client agent-node "ToolsAgent")
            ^String prompt (or input "What is 5 plus 3?")
            {:keys [tool-calls text]}
            (model/chat m {:messages [(model/user prompt)]
                           :tools    [CALCULATOR-TOOL]})]
        (if (seq tool-calls)
          (let [results (aor/agent-invoke tools-agent tool-calls)]
            (aor/result! agent-node
                         {"mode"         "tool-call"
                          "tool-calls"   (count tool-calls)
                          "tool-results" results}))
          (aor/result! agent-node
                       {"mode"     "tool-call"
                        "response" text})))

      :store
      ;; Use key-value store
      (let [kv-store (aor/get-store agent-node "$$test-store")
            key      (or input "test-key")
            _ (store/put! kv-store key (str "value-" key))
            value    (store/get kv-store key)]
        (aor/result!
         agent-node
         {"mode" "store" "key" key "value" value}))

      :db
      ;; Simulate DB operation with record-nested-op!
      (let [start-time   (System/currentTimeMillis)
            _ (Thread/sleep 10)
            query-result {"rows" 42 "query" "SELECT * FROM test"}
            finish-time  (System/currentTimeMillis)]
        (aor/record-nested-op!
         agent-node
         :db-read
         start-time
         finish-time
         {"query"  "SELECT * FROM test"
          "rows"   42
          "result" "success"})
        (aor/result!
         agent-node
         {"mode" "db" "query-result" query-result}))

      :other
      ;; Simulate other operation with record-nested-op!
      (let [start-time  (System/currentTimeMillis)
            _ (Thread/sleep 10)
            op-result   {"status" "completed" "data" input}
            finish-time (System/currentTimeMillis)]
        (aor/record-nested-op! agent-node
                               :other
                               start-time
                               finish-time
                               {"operation" "custom-processing"
                                "status"    "completed"
                                "input"     (str input)})
        (aor/result! agent-node {"mode" "other" "op-result" op-result}))

      ;; Default
      (aor/result! agent-node
                   {"mode"  "unknown"
                    "error" (str "Unknown mode: " mode)}))))

;;; Test agent module
(aor/defagentmodule TraceAnalyticsTestAgentModule
  [topology]

  ;; OpenAI model for :chat and :tool-call modes
  (aor/declare-agent-object-builder
   topology
   "openai-model"
   (fn [_setup]
     (openai/responses-model
      {:api-key (or (System/getenv "OPENAI_API_KEY") "fake-key")
       :model   "gpt-4o-mini"})))

  ;; Key-value store for :store mode
  (aor/declare-key-value-store topology "$$test-store" String String)

  ;; Basic sub-agent for :sub-agent mode
  (-> topology
      (aor/new-agent "BasicSubAgent")
      (aor/node
       "process"
       nil
       (fn [agent-node input]
         (aor/result! agent-node (str "SubAgent processed: " input)))))

  ;; Tools agent for :tool-call mode
  (tools/new-tools-agent topology "ToolsAgent" [CALCULATOR-TOOL])

  ;; Main test agent with mode-based execution
  (-> topology
      (aor/new-agent "TraceTestAgent")
      (aor/node
       "execute"
       nil
       (fn [agent-node m]
         ;; wrapper so we can modify the test-agent at the repl
         (test-agent agent-node m)))))
