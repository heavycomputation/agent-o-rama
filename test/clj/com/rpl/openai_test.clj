(ns com.rpl.openai-test
  "Pure translation tests for the OpenAI Responses API adapter — no network."
  (:use [clojure.test])
  (:require
   [com.rpl.agent-o-rama.impl.openai :as iopenai]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [jsonista.core :as j]))

(def ADD-SCHEMA
  (schema/object
   {:required ["a" "b"]}
   {"a" (schema/number "first number")
    "b" (schema/number "second number")}))

(def ADD-TOOL
  (tools/tool
   {:name        "add"
    :description "Add two numbers"
    :schema      ADD-SCHEMA}
   (fn [args] (+ (get args "a") (get args "b")))))

(deftest request->wire-test
  (let [wire (iopenai/request->wire
              {:model     "gpt-5.1"
               :reasoning {:effort :medium}
               :store?    false}
              {:messages          [(m/system "sys prompt")
                                   (m/user "hi")]
               :tools             [ADD-TOOL (openai/web-search)]
               :tool-choice       :auto
               :temperature       0.2
               :max-output-tokens 100})]
    (is (= "gpt-5.1" (get wire "model")))
    (is (= [{"role" "system" "content" "sys prompt"}
            {"role" "user" "content" "hi"}]
           (get wire "input")))
    (is (= [{"type"        "function"
             "name"        "add"
             "description" "Add two numbers"
             "parameters"  ADD-SCHEMA}
            {"type" "web_search"}]
           (get wire "tools")))
    (is (= "auto" (get wire "tool_choice")))
    (is (= 0.2 (get wire "temperature")))
    (is (= 100 (get wire "max_output_tokens")))
    (is (= {"effort" "medium"} (get wire "reasoning")))
    (is (false? (get wire "store")))
    (is (= ["reasoning.encrypted_content"] (get wire "include")))))

(deftest request->wire-overrides-test
  (let [defaults {:model "gpt-5.1" :reasoning {:effort :medium} :store? false}]
    ;; per-request model + reasoning override
    (let [wire (iopenai/request->wire
                defaults
                {:messages  [(m/user "hi")]
                 :model     "gpt-5.1-mini"
                 :reasoning {:effort :high :summary :auto}})]
      (is (= "gpt-5.1-mini" (get wire "model")))
      (is (= {"effort" "high" "summary" "auto"} (get wire "reasoning"))))
    ;; reasoning can be disabled per request
    (let [wire (iopenai/request->wire defaults
                                      {:messages  [(m/user "hi")]
                                       :reasoning nil})]
      (is (nil? (get wire "reasoning"))))
    ;; store? true drops the automatic encrypted-content include
    (let [wire (iopenai/request->wire defaults
                                      {:messages [(m/user "hi")]
                                       :store?   true})]
      (is (true? (get wire "store")))
      (is (nil? (get wire "include"))))))

(deftest output-schema-test
  ;; bare schema map: default name + strict
  (let [wire (iopenai/request->wire
              {:model "gpt-5.1"}
              {:messages      [(m/user "hi")]
               :output-schema {"type" "object"}})]
    (is (= {"format" {"type"   "json_schema"
                      "name"   "response"
                      "schema" {"type" "object"}
                      "strict" true}}
           (get wire "text"))))
  ;; named + strictness controlled
  (let [wire (iopenai/request->wire
              {:model "gpt-5.1"}
              {:messages      [(m/user "hi")]
               :output-schema {:name    "analysis"
                               :schema  {"type" "object"}
                               :strict? false}})]
    (is (= {"format" {"type"   "json_schema"
                      "name"   "analysis"
                      "schema" {"type" "object"}
                      "strict" false}}
           (get wire "text")))))

(def WIRE-RESPONSE
  {"id"     "resp_123"
   "model"  "gpt-5.1"
   "status" "completed"
   "output" [{"type"              "reasoning"
              "id"                "rs_1"
              "summary"           [{"type" "summary_text"
                                    "text" "Thinking about math."}]
              "encrypted_content" "ENCRYPTED"}
             {"type"    "message"
              "id"      "msg_1"
              "role"    "assistant"
              "content" [{"type"        "output_text"
                          "text"        "I'll use the add tool."
                          "annotations" []}]}
             {"type"      "function_call"
              "id"        "fc_1"
              "call_id"   "call_1"
              "name"      "add"
              "arguments" "{\"a\":1,\"b\":2}"}]
   "usage"  {"input_tokens"          100
             "input_tokens_details"  {"cached_tokens" 50}
             "output_tokens"         30
             "output_tokens_details" {"reasoning_tokens" 20}
             "total_tokens"          130}})

(deftest response->neutral-test
  (let [{:keys [message text tool-calls finish-reason usage model response-id]}
        (iopenai/response->neutral WIRE-RESPONSE)]
    (is (= [{:type :reasoning :summary "Thinking about math."}
            {:type :text :text "I'll use the add tool."}
            {:type :tool-call :id "call_1" :name "add"
             :args {"a" 1 "b" 2}}]
           (:content message)))
    (is (= :assistant (:role message)))
    (is (= "I'll use the add tool." text))
    (is (= [{:id "call_1" :name "add" :args {"a" 1 "b" 2}}] tool-calls))
    (is (= :tool-calls finish-reason))
    (is (= {:input-tokens        100
            :output-tokens       30
            :total-tokens        130
            :reasoning-tokens    20
            :cached-input-tokens 50}
           usage))
    (is (= "gpt-5.1" model))
    (is (= "resp_123" response-id))))

(deftest response->neutral-finish-reasons-test
  (let [text-only {"id"     "resp_1"
                   "status" "completed"
                   "output" [{"type"    "message"
                              "role"    "assistant"
                              "content" [{"type" "output_text"
                                          "text" "done"}]}]}]
    (is (= :stop (:finish-reason (iopenai/response->neutral text-only))))
    (is (= :length
           (:finish-reason
            (iopenai/response->neutral
             (assoc text-only
                    "status" "incomplete"
                    "incomplete_details" {"reason" "max_output_tokens"})))))
    (is (= :refusal
           (:finish-reason
            (iopenai/response->neutral
             (assoc text-only "output"
                    [{"type"    "message"
                      "role"    "assistant"
                      "content" [{"type" "refusal"
                                  "refusal" "no"}]}])))))))

(deftest reasoning-round-trip-test
  ;; the whole point: an adapter-produced assistant message replays its raw
  ;; output items (reasoning + encrypted content included) verbatim, followed
  ;; by the tool result
  (let [{:keys [message tool-calls]} (iopenai/response->neutral WIRE-RESPONSE)
        tool-result (m/tool-result (first tool-calls) 3)
        input       (iopenai/messages->input
                     [(m/user "add 1 and 2") message tool-result])]
    (is (= (into [{"role" "user" "content" "add 1 and 2"}]
                 (get WIRE-RESPONSE "output"))
           (butlast input)))
    (is (= {"type"    "function_call_output"
            "call_id" "call_1"
            "output"  "3"}
           (last input)))))

(deftest hand-built-assistant-test
  ;; assistant messages constructed by hand (no provider data) are converted
  ;; from their blocks
  (let [input (iopenai/messages->input
               [(m/assistant [{:type :text :text "previous answer"}
                              {:type :tool-call :id "c9" :name "add"
                               :args {"a" 1 "b" 1}}])])]
    (is (= [{"type"    "message"
             "role"    "assistant"
             "content" [{"type"        "output_text"
                         "text"        "previous answer"
                         "annotations" []}]}
            {"type"      "function_call"
             "call_id"   "c9"
             "name"      "add"
             "arguments" "{\"a\":1,\"b\":1}"}]
           input))))

(deftest maybe-parse-output-test
  (let [response {:text "{\"x\": 5}"}]
    (is (= {"x" 5}
           (:parsed (iopenai/maybe-parse-output
                     response
                     {:output-schema {"type" "object"}}))))
    (is (nil? (:parsed (iopenai/maybe-parse-output response {}))))))

(deftest process-sse-lines-test
  (let [completed {"id"     "resp_1"
                   "status" "completed"
                   "output" [{"type"    "message"
                              "role"    "assistant"
                              "content" [{"type" "output_text"
                                          "text" "Hey"}]}]}
        lines  [(str "data: "
                     (j/write-value-as-string
                      {"type" "response.output_text.delta" "delta" "He"}))
                ""
                "data: {\"type\":\"response.output_text.delta\",\"delta\":\"y\"}"
                "data: {\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"think\"}"
                "data: {\"type\":\"response.output_item.done\",\"item\":{}}"
                (str "data: "
                     (j/write-value-as-string
                      {"type" "response.completed" "response" completed}))
                "data: [DONE]"]
        events (atom [])
        final  (iopenai/process-sse-lines lines #(swap! events conj %))]
    (is (= completed final))
    (is (= [{:type :text-delta :text "He"}
            {:type :text-delta :text "y"}
            {:type :reasoning-delta :text "think"}]
           @events))))

(deftest process-sse-lines-error-test
  (is (thrown-with-msg?
       Exception #"OpenAI streaming request failed"
       (iopenai/process-sse-lines
        ["data: {\"type\":\"error\",\"message\":\"bad\"}"]
        (fn [_]))))
  (is (thrown-with-msg?
       Exception #"without a complete response"
       (iopenai/process-sse-lines
        ["data: {\"type\":\"response.output_text.delta\",\"delta\":\"x\"}"]
        (fn [_])))))

(deftest lc4j-tool-rejected-test
  ;; lc4j ToolSpecification-based tools get a clear error from the native
  ;; adapter rather than a confusing one
  (is (thrown-with-msg?
       Exception #"use com.rpl.agent-o-rama.tools/tool"
       (iopenai/tool->wire
        (tools/tool-info
         (tools/tool-specification "add" nil "Add")
         (fn [args] args))))))
