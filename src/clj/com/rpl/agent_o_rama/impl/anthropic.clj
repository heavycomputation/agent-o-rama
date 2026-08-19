(ns com.rpl.agent-o-rama.impl.anthropic
  "Anthropic Messages API integration internals: translation between the
  provider-neutral formats (com.rpl.agent-o-rama.model) and the Messages API
  wire format, plus SSE stream accumulation.

  Assistant messages produced by this adapter carry the raw content blocks
  under [:provider-data :anthropic-content]. Replaying such a message sends
  the raw blocks — thinking blocks with their signatures included — back
  verbatim, as the API requires for extended thinking across tool use."
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.model-http :as mhttp]
   [com.rpl.agent-o-rama.model :as model])
  (:import
   [java.util Base64]))

(def ^:private json-decode mhttp/json-decode)

;;; ---------------------------------------------------------------------------
;;; request translation: neutral -> wire
;;; ---------------------------------------------------------------------------

(defn- user-content-block->wire
  [block]
  (case (:type block)
    :text  {"type" "text"
            "text" (:text block)}
    :image {"type"   "image"
            "source" (if (:url block)
                       {"type" "url"
                        "url"  (:url block)}
                       {"type"       "base64"
                        "media_type" (:mime-type block)
                        "data"       (.encodeToString (Base64/getEncoder)
                                                      ^bytes (:data block))})}
    (throw (h/ex-info "Unsupported user content block for Anthropic"
                      {:block-type (:type block)}))))

(defn- user-content->wire
  [content]
  (if (string? content)
    content
    (mapv user-content-block->wire (model/content-blocks content))))

(defn- assistant-content->wire
  [message]
  (if-let [raw-blocks (-> message :provider-data :anthropic-content)]
    ;; adapter-produced message: replay raw content blocks verbatim
    ;; (thinking blocks with signatures included)
    (vec raw-blocks)
    (into []
          (keep (fn [block]
                  (case (:type block)
                    :text      {"type" "text"
                                "text" (:text block)}
                    :tool-call {"type"  "tool_use"
                                "id"    (:id block)
                                "name"  (:name block)
                                "input" (:args block)}
                    ;; thinking can't be reconstructed without provider data
                    nil)))
          (model/content-blocks (:content message)))))

(defn- tool-result->wire-block
  [message]
  {"type"        "tool_result"
   "tool_use_id" (:tool-call-id message)
   "content"     (str (model/content-text (:content message)))})

(defn messages->wire
  "Translates neutral messages to the Messages API shape: returns
  {:system <string or nil> :messages [...]}. System messages are hoisted to
  the top-level system param; consecutive :tool messages are merged into a
  single user turn of tool_result blocks, as the API requires."
  [messages]
  (let [system   (let [texts (into []
                                   (comp (filter #(= :system (:role %)))
                                         (keep #(model/content-text (:content %))))
                                   messages)]
                   (when (seq texts)
                     (str/join "\n\n" texts)))
        non-system (remove #(= :system (:role %)) messages)
        wire     (reduce
                  (fn [acc {:keys [role] :as message}]
                    (case role
                      :user
                      (conj acc {"role"    "user"
                                 "content" (user-content->wire
                                            (:content message))})

                      :assistant
                      (conj acc {"role"    "assistant"
                                 "content" (assistant-content->wire message)})

                      :tool
                      (let [block (tool-result->wire-block message)
                            prev  (peek acc)]
                        ;; merge consecutive tool results into one user turn
                        (if (and prev
                                 (= "user" (get prev "role"))
                                 (vector? (get prev "content"))
                                 (= "tool_result"
                                    (get (peek (get prev "content")) "type")))
                          (conj (pop acc)
                                (update prev "content" conj block))
                          (conj acc {"role"    "user"
                                     "content" [block]})))

                      (throw (h/ex-info "Unknown message role" {:role role}))))
                  []
                  non-system)]
    {:system   system
     :messages wire}))

(defn tool->wire
  [tool]
  (let [spec (or (:tool-specification tool) tool)]
    (cond
      ;; server tools ({:type "web_search_20250305" ...}) pass through
      (and (map? spec)
           (or (contains? spec :type) (contains? spec "type")))
      (into {}
            (map (fn [[k v]] [(if (keyword? k) (name k) k) v]))
            spec)

      (and (map? spec) (string? (:name spec)))
      (h/remove-empty-vals
       {"name"         (:name spec)
        "description"  (:description spec)
        "input_schema" (:schema spec)})

      :else
      (throw (h/ex-info
              "Cannot convert tool for the Anthropic Messages API — use com.rpl.agent-o-rama.tools/tool (data specs)"
              {:tool-class (class spec)})))))

(defn- tool-choice->wire
  [tool-choice]
  (cond
    (= :auto tool-choice)     {"type" "auto"}
    (= :required tool-choice) {"type" "any"}
    (= :none tool-choice)     {"type" "none"}
    (map? tool-choice)        {"type" "tool"
                               "name" (:name tool-choice)}
    :else                     tool-choice))

(defn- thinking->wire
  [thinking]
  (when thinking
    {"type"          "enabled"
     "budget_tokens" (:budget-tokens thinking)}))

(defn- output-schema->wire
  [output-schema]
  (let [schema (if (contains? output-schema :schema)
                 (:schema output-schema)
                 output-schema)]
    {"type"   "json_schema"
     "schema" schema}))

(defn request->wire
  "Translates a neutral request map to a Messages API request body.
  defaults come from the model config; per-request keys win."
  [{default-model :model
    default-thinking :thinking
    default-max-tokens :max-tokens}
   request]
  (let [thinking (if (contains? request :thinking)
                   (:thinking request)
                   default-thinking)
        {:keys [system messages]} (messages->wire (:messages request))]
    (h/remove-empty-vals
     {"model"          (or (:model request) default-model)
      "system"         system
      "messages"       messages
      "max_tokens"     (or (:max-output-tokens request)
                           default-max-tokens
                           4096)
      "tools"          (some->> (:tools request) (mapv tool->wire))
      "tool_choice"    (some-> (:tool-choice request) tool-choice->wire)
      "temperature"    (:temperature request)
      "top_p"          (:top-p request)
      "top_k"          (:top-k request)
      "stop_sequences" (some-> (:stop-sequences request) vec)
      "thinking"       (thinking->wire thinking)
      "output_format"  (some-> (:output-schema request) output-schema->wire)
      "metadata"       (:metadata request)})))

;;; ---------------------------------------------------------------------------
;;; response translation: wire -> neutral
;;; ---------------------------------------------------------------------------

(defn- content-block->neutral
  [block]
  (case (get block "type")
    "thinking"          {:type    :reasoning
                         :summary (get block "thinking")}
    "redacted_thinking" {:type :reasoning}
    "text"              {:type :text
                         :text (get block "text")}
    "tool_use"          {:type :tool-call
                         :id   (get block "id")
                         :name (get block "name")
                         :args (get block "input")}
    ;; server tool blocks (server_tool_use, web_search_tool_result, ...)
    {:type :built-in-tool-call
     :item block}))

(defn- stop-reason->finish
  [stop-reason]
  (cond
    (= "refusal" stop-reason)    :refusal
    (= "max_tokens" stop-reason) :length
    (= "tool_use" stop-reason)   :tool-calls
    (contains? #{"end_turn" "stop_sequence"} stop-reason) :stop
    :else :other))

(defn response->neutral
  "Translates a Messages API response object (string-keyed map) to the
  neutral response format."
  [wire]
  (let [raw-blocks (vec (get wire "content"))
        blocks     (mapv content-block->neutral raw-blocks)
        text       (let [texts (into []
                                     (comp (filter #(= :text (:type %)))
                                           (keep :text))
                                     blocks)]
                     (when (seq texts)
                       (apply str texts)))
        tool-calls (into []
                         (comp (filter #(= :tool-call (:type %)))
                               (map #(select-keys % [:id :name :args])))
                         blocks)
        usage      (get wire "usage")
        input-tokens  (get usage "input_tokens")
        output-tokens (get usage "output_tokens")]
    (h/remove-empty-vals
     {:message       {:role          :assistant
                      :content       blocks
                      :provider-data {:anthropic-content raw-blocks}}
      :text          text
      :tool-calls    tool-calls
      :finish-reason (stop-reason->finish (get wire "stop_reason"))
      :usage         (h/remove-empty-vals
                      {:input-tokens        input-tokens
                       :output-tokens       output-tokens
                       :total-tokens        (when (and input-tokens
                                                       output-tokens)
                                              (+ input-tokens output-tokens))
                       :cached-input-tokens (get usage
                                                 "cache_read_input_tokens")})
      :model         (get wire "model")
      :response-id   (get wire "id")})))

(defn maybe-parse-output
  [response request]
  (if (and (:output-schema request) (:text response))
    (assoc response :parsed (json-decode (:text response)))
    response))

;;; ---------------------------------------------------------------------------
;;; SSE stream accumulation
;;; ---------------------------------------------------------------------------

(defn process-sse-lines
  "Processes Messages API SSE lines, invoking on-event with neutral delta
  events as they occur, and accumulating the full response. Returns the
  final response object (wire format, same shape as a non-streaming
  response). Throws on stream errors."
  [lines on-event]
  (let [state (volatile! {:message nil
                          :blocks  {}       ; index -> content block
                          :json    {}       ; index -> accumulated partial json
                          :error   nil})]
    (mhttp/each-sse-payload
     lines
     (fn [event]
       (case (get event "type")
         "message_start"
         (vswap! state assoc :message (get event "message"))

         "content_block_start"
         (vswap! state assoc-in
                 [:blocks (get event "index")]
                 (get event "content_block"))

         "content_block_delta"
         (let [index (get event "index")
               delta (get event "delta")]
           (case (get delta "type")
             "text_delta"
             (do
               (on-event {:type :text-delta
                          :text (get delta "text")})
               (vswap! state update-in [:blocks index "text"]
                       str (get delta "text")))

             "thinking_delta"
             (do
               (on-event {:type :reasoning-delta
                          :text (get delta "thinking")})
               (vswap! state update-in [:blocks index "thinking"]
                       str (get delta "thinking")))

             "input_json_delta"
             (do
               (on-event {:type :tool-call-delta
                          :text (get delta "partial_json")})
               (vswap! state update-in [:json index]
                       str (get delta "partial_json")))

             "signature_delta"
             (vswap! state update-in [:blocks index "signature"]
                     str (get delta "signature"))

             nil))

         "content_block_stop"
         (let [index (get event "index")
               json  (get-in @state [:json index])]
           ;; tool_use inputs stream as partial json; parse at block end
           (when (seq json)
             (vswap! state assoc-in [:blocks index "input"]
                     (json-decode json))))

         "message_delta"
         (vswap! state
                 (fn [s]
                   (-> s
                       (update :message merge (get event "delta"))
                       (update-in [:message "usage"]
                                  merge (get event "usage")))))

         "error"
         (vswap! state assoc :error (get event "error" event))

         ;; message_stop, ping
         nil)))
    (let [{:keys [message blocks error]} @state]
      (when error
        (throw (h/ex-info "Anthropic streaming request failed" {:error error})))
      (when-not message
        (throw (h/ex-info "Anthropic stream ended without a response" {})))
      (assoc message "content"
             (into []
                   (map val)
                   (sort-by key blocks))))))
