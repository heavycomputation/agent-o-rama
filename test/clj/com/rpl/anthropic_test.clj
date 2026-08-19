(ns com.rpl.anthropic-test
  "Pure translation tests for the Anthropic Messages API adapter — no network."
  (:use [clojure.test])
  (:require
   [com.rpl.agent-o-rama.impl.anthropic :as ianthropic]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.anthropic :as anthropic]
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
  (let [wire (ianthropic/request->wire
              {:model      "claude-sonnet-5"
               :thinking   {:budget-tokens 2048}
               :max-tokens 1024}
              {:messages    [(m/system "be terse")
                             (m/system "be kind")
                             (m/user "hi")]
               :tools       [ADD-TOOL (anthropic/web-search {:max-uses 2})]
               :tool-choice :auto
               :temperature 0.5})]
    (is (= "claude-sonnet-5" (get wire "model")))
    ;; system messages hoisted out of the message list
    (is (= "be terse\n\nbe kind" (get wire "system")))
    (is (= [{"role" "user" "content" "hi"}] (get wire "messages")))
    (is (= 1024 (get wire "max_tokens")))
    (is (= [{"name"         "add"
             "description"  "Add two numbers"
             "input_schema" (:schema (:tool-specification ADD-TOOL))}
            {"type"     "web_search_20250305"
             "name"     "web_search"
             "max_uses" 2}]
           (get wire "tools")))
    (is (= {"type" "auto"} (get wire "tool_choice")))
    (is (= {"type" "enabled" "budget_tokens" 2048} (get wire "thinking")))
    (is (= 0.5 (get wire "temperature")))))

(deftest tool-results-merge-test
  ;; consecutive :tool messages become ONE user turn of tool_result blocks
  (let [{:keys [messages]}
        (ianthropic/messages->wire
         [(m/user "add stuff")
          (m/assistant [{:type :tool-call :id "t1" :name "add"
                         :args {"a" 1 "b" 2}}
                        {:type :tool-call :id "t2" :name "add"
                         :args {"a" 3 "b" 4}}])
          (m/tool-result {:id "t1" :name "add"} 3)
          (m/tool-result {:id "t2" :name "add"} 7)])]
    (is (= 3 (count messages)))
    (is (= {"role"    "assistant"
            "content" [{"type" "tool_use" "id" "t1" "name" "add"
                        "input" {"a" 1 "b" 2}}
                       {"type" "tool_use" "id" "t2" "name" "add"
                        "input" {"a" 3 "b" 4}}]}
           (nth messages 1)))
    (is (= {"role"    "user"
            "content" [{"type" "tool_result" "tool_use_id" "t1"
                        "content" "3"}
                       {"type" "tool_result" "tool_use_id" "t2"
                        "content" "7"}]}
           (nth messages 2)))))

(def WIRE-RESPONSE
  {"id"          "msg_123"
   "model"       "claude-sonnet-5"
   "role"        "assistant"
   "stop_reason" "tool_use"
   "content"     [{"type"      "thinking"
                   "thinking"  "Let me add these."
                   "signature" "SIG"}
                  {"type" "text"
                   "text" "Adding now."}
                  {"type"  "tool_use"
                   "id"    "toolu_1"
                   "name"  "add"
                   "input" {"a" 1 "b" 2}}]
   "usage"       {"input_tokens"            50
                  "output_tokens"           20
                  "cache_read_input_tokens" 10}})

(deftest response->neutral-test
  (let [{:keys [message text tool-calls finish-reason usage model response-id]}
        (ianthropic/response->neutral WIRE-RESPONSE)]
    (is (= [{:type :reasoning :summary "Let me add these."}
            {:type :text :text "Adding now."}
            {:type :tool-call :id "toolu_1" :name "add"
             :args {"a" 1 "b" 2}}]
           (:content message)))
    (is (= "Adding now." text))
    (is (= [{:id "toolu_1" :name "add" :args {"a" 1 "b" 2}}] tool-calls))
    (is (= :tool-calls finish-reason))
    (is (= {:input-tokens        50
            :output-tokens       20
            :total-tokens        70
            :cached-input-tokens 10}
           usage))
    (is (= "claude-sonnet-5" model))
    (is (= "msg_123" response-id))))

(deftest thinking-round-trip-test
  ;; adapter-produced assistant messages replay raw content blocks verbatim —
  ;; thinking blocks with signatures included
  (let [{:keys [message tool-calls]} (ianthropic/response->neutral
                                      WIRE-RESPONSE)
        {:keys [messages]}
        (ianthropic/messages->wire
         [(m/user "add 1 and 2")
          message
          (m/tool-result (first tool-calls) 3)])]
    (is (= {"role"    "assistant"
            "content" (get WIRE-RESPONSE "content")}
           (nth messages 1)))
    (is (= {"role"    "user"
            "content" [{"type" "tool_result" "tool_use_id" "toolu_1"
                        "content" "3"}]}
           (nth messages 2)))))

(deftest finish-reasons-test
  (letfn [(finish [stop-reason]
            (:finish-reason
             (ianthropic/response->neutral
              (assoc WIRE-RESPONSE
                     "stop_reason" stop-reason
                     "content" [{"type" "text" "text" "x"}]))))]
    (is (= :stop (finish "end_turn")))
    (is (= :stop (finish "stop_sequence")))
    (is (= :length (finish "max_tokens")))
    (is (= :refusal (finish "refusal")))))

(deftest process-sse-lines-test
  (let [event (fn [payload]
                (str "data: " (j/write-value-as-string payload)))
        lines  [(event {"type"    "message_start"
                        "message" {"id"    "msg_1"
                                   "model" "claude-sonnet-5"
                                   "role"  "assistant"
                                   "usage" {"input_tokens" 40}}})
                (event {"type"          "content_block_start"
                        "index"         0
                        "content_block" {"type" "thinking"
                                         "thinking" ""}})
                (event {"type"  "content_block_delta"
                        "index" 0
                        "delta" {"type" "thinking_delta"
                                 "thinking" "hmm"}})
                (event {"type"  "content_block_delta"
                        "index" 0
                        "delta" {"type" "signature_delta"
                                 "signature" "SIG"}})
                (event {"type" "content_block_stop" "index" 0})
                (event {"type"          "content_block_start"
                        "index"         1
                        "content_block" {"type" "text" "text" ""}})
                (event {"type"  "content_block_delta"
                        "index" 1
                        "delta" {"type" "text_delta" "text" "Hel"}})
                (event {"type"  "content_block_delta"
                        "index" 1
                        "delta" {"type" "text_delta" "text" "lo"}})
                (event {"type" "content_block_stop" "index" 1})
                (event {"type"          "content_block_start"
                        "index"         2
                        "content_block" {"type" "tool_use" "id" "toolu_9"
                                         "name" "add"}})
                (event {"type"  "content_block_delta"
                        "index" 2
                        "delta" {"type" "input_json_delta"
                                 "partial_json" "{\"a\": 1,"}})
                (event {"type"  "content_block_delta"
                        "index" 2
                        "delta" {"type" "input_json_delta"
                                 "partial_json" " \"b\": 2}"}})
                (event {"type" "content_block_stop" "index" 2})
                (event {"type"  "message_delta"
                        "delta" {"stop_reason" "tool_use"}
                        "usage" {"output_tokens" 15}})
                (event {"type" "message_stop"})]
        events (atom [])
        final  (ianthropic/process-sse-lines lines #(swap! events conj %))]
    (is (= {"id"          "msg_1"
            "model"       "claude-sonnet-5"
            "role"        "assistant"
            "stop_reason" "tool_use"
            "usage"       {"input_tokens" 40 "output_tokens" 15}
            "content"     [{"type" "thinking" "thinking" "hmm"
                            "signature" "SIG"}
                           {"type" "text" "text" "Hello"}
                           {"type" "tool_use" "id" "toolu_9" "name" "add"
                            "input" {"a" 1 "b" 2}}]}
           final))
    (is (= [{:type :reasoning-delta :text "hmm"}
            {:type :text-delta :text "Hel"}
            {:type :text-delta :text "lo"}
            {:type :tool-call-delta :text "{\"a\": 1,"}
            {:type :tool-call-delta :text " \"b\": 2}"}]
           @events))
    ;; and the accumulated response translates like a non-streaming one
    (let [{:keys [text tool-calls finish-reason]}
          (ianthropic/response->neutral final)]
      (is (= "Hello" text))
      (is (= [{:id "toolu_9" :name "add" :args {"a" 1 "b" 2}}] tool-calls))
      (is (= :tool-calls finish-reason)))))
