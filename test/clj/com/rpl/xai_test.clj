(ns com.rpl.xai-test
  "Pure translation tests for the xAI adapter — no network."
  (:use [clojure.test])
  (:require
   [com.rpl.agent-o-rama.impl.xai :as ixai]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [jsonista.core :as j]))

(def ADD-TOOL
  (tools/tool
   {:name        "add"
    :description "Add two numbers"
    :schema      (schema/object
                  {:required ["a" "b"]}
                  {"a" (schema/number)
                   "b" (schema/number)})}
   (fn [args] (+ (get args "a") (get args "b")))))

(deftest request->wire-test
  (let [wire (ixai/request->wire
              {:model             "grok-4-1"
               :search-parameters {:mode "auto"
                                   :sources [{:type "web"} {:type "x"}]}}
              {:messages          [(m/system "be terse")
                                   (m/user "hi")]
               :tools             [ADD-TOOL]
               :tool-choice       :auto
               :temperature       0.3
               :max-output-tokens 200})]
    (is (= "grok-4-1" (get wire "model")))
    (is (= [{"role" "system" "content" "be terse"}
            {"role" "user" "content" "hi"}]
           (get wire "messages")))
    (is (= [{"type"     "function"
             "function" {"name"        "add"
                         "description" "Add two numbers"
                         "parameters"  (:schema (:tool-specification ADD-TOOL))}}]
           (get wire "tools")))
    (is (= "auto" (get wire "tool_choice")))
    (is (= {"mode" "auto"
            "sources" [{"type" "web"} {"type" "x"}]}
           (get wire "search_parameters")))
    (is (= 200 (get wire "max_tokens")))))

(deftest output-schema-and-effort-test
  (let [wire (ixai/request->wire
              {:model "grok-3-mini" :reasoning-effort :low}
              {:messages      [(m/user "hi")]
               :output-schema {:name    "math"
                               :schema  {"type" "object"}
                               :strict? true}})]
    (is (= {"type"        "json_schema"
            "json_schema" {"name"   "math"
                           "schema" {"type" "object"}
                           "strict" true}}
           (get wire "response_format")))
    (is (= "low" (get wire "reasoning_effort")))))

(def WIRE-RESPONSE
  {"id"        "cmpl_1"
   "model"     "grok-4-1"
   "citations" ["https://example.com/a"]
   "choices"   [{"finish_reason" "tool_calls"
                 "message"
                 {"role"              "assistant"
                  "content"           "Let me add those."
                  "reasoning_content" "The user wants addition."
                  "tool_calls"        [{"id"       "call_1"
                                        "type"     "function"
                                        "function" {"name"      "add"
                                                    "arguments" "{\"a\":1,\"b\":2}"}}]}}]
   "usage"     {"prompt_tokens"     30
                "completion_tokens" 12
                "total_tokens"      42
                "completion_tokens_details" {"reasoning_tokens" 7}}})

(deftest response->neutral-test
  (let [{:keys [message text tool-calls citations finish-reason usage model
                response-id]}
        (ixai/response->neutral WIRE-RESPONSE)]
    (is (= [{:type :reasoning :summary "The user wants addition."}
            {:type :text :text "Let me add those."}
            {:type :tool-call :id "call_1" :name "add"
             :args {"a" 1 "b" 2}}]
           (:content message)))
    (is (= "Let me add those." text))
    (is (= [{:id "call_1" :name "add" :args {"a" 1 "b" 2}}] tool-calls))
    (is (= ["https://example.com/a"] citations))
    (is (= :tool-calls finish-reason))
    (is (= {:input-tokens     30
            :output-tokens    12
            :total-tokens     42
            :reasoning-tokens 7}
           usage))
    (is (= "grok-4-1" model))
    (is (= "cmpl_1" response-id))))

(deftest replay-drops-reasoning-test
  ;; adapter-produced assistant messages replay content + tool_calls but
  ;; never reasoning_content
  (let [{:keys [message tool-calls]} (ixai/response->neutral WIRE-RESPONSE)
        wire (ixai/request->wire
              {:model "grok-4-1"}
              {:messages [(m/user "add 1 and 2")
                          message
                          (m/tool-result (first tool-calls) 3)]})]
    (is (= {"role"       "assistant"
            "content"    "Let me add those."
            "tool_calls" [{"id"       "call_1"
                           "type"     "function"
                           "function" {"name"      "add"
                                       "arguments" "{\"a\":1,\"b\":2}"}}]}
           (nth (get wire "messages") 1)))
    (is (= {"role"         "tool"
            "tool_call_id" "call_1"
            "content"      "3"}
           (nth (get wire "messages") 2)))))

(deftest process-sse-lines-test
  (let [chunk (fn [payload]
                (str "data: " (j/write-value-as-string payload)))
        lines [(chunk {"id"      "cmpl_9"
                       "model"   "grok-4-1"
                       "choices" [{"delta" {"reasoning_content" "think"}}]})
               (chunk {"id"      "cmpl_9"
                       "choices" [{"delta" {"content" "Hel"}}]})
               (chunk {"id"      "cmpl_9"
                       "choices" [{"delta" {"content" "lo"}}]})
               (chunk {"id"      "cmpl_9"
                       "choices" [{"delta"
                                   {"tool_calls"
                                    [{"index"    0
                                      "id"       "call_7"
                                      "function" {"name"      "add"
                                                  "arguments" "{\"a\":"}}]}}]})
               (chunk {"id"      "cmpl_9"
                       "choices" [{"delta"
                                   {"tool_calls"
                                    [{"index"    0
                                      "function" {"arguments" "1}"}}]}
                                   "finish_reason" "tool_calls"}]})
               (chunk {"id"      "cmpl_9"
                       "choices" []
                       "usage"   {"prompt_tokens"     5
                                  "completion_tokens" 3
                                  "total_tokens"      8}})
               "data: [DONE]"]
        events (atom [])
        final  (ixai/process-sse-lines lines #(swap! events conj %))]
    (is (= [{:type :reasoning-delta :text "think"}
            {:type :text-delta :text "Hel"}
            {:type :text-delta :text "lo"}
            {:type :tool-call-delta :text "{\"a\":"}
            {:type :tool-call-delta :text "1}"}]
           @events))
    (let [{:keys [text tool-calls finish-reason usage message]}
          (ixai/response->neutral final)]
      (is (= "Hello" text))
      (is (= [{:id "call_7" :name "add" :args {"a" 1}}] tool-calls))
      (is (= :tool-calls finish-reason))
      (is (= {:input-tokens 5 :output-tokens 3 :total-tokens 8} usage))
      (is (= "think" (:summary (first (:content message))))))))
