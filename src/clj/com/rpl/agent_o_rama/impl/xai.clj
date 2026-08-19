(ns com.rpl.agent-o-rama.impl.xai
  "xAI (Grok) API integration internals: translation between the
  provider-neutral formats (com.rpl.agent-o-rama.model) and xAI's
  chat-completions wire format (with xAI extensions: reasoning_content,
  search_parameters / Live Search citations), plus SSE stream accumulation.

  Assistant messages produced by this adapter carry the raw wire message
  under [:provider-data :xai-message]; replaying one sends its content and
  tool_calls back (reasoning_content is never replayed, per the API)."
  (:require
   [clojure.walk :as walk]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.model-http :as mhttp]
   [com.rpl.agent-o-rama.model :as model]))

(def ^:private json-encode mhttp/json-encode)
(def ^:private json-decode mhttp/json-decode)

(defn- stringify-keys-deep
  [x]
  (walk/postwalk
   (fn [v]
     (if (map? v)
       (into {}
             (map (fn [[k val]] [(if (keyword? k) (name k) k) val]))
             v)
       v))
   x))

;;; ---------------------------------------------------------------------------
;;; request translation: neutral -> wire
;;; ---------------------------------------------------------------------------

(defn- user-content-block->wire
  [block]
  (case (:type block)
    :text  {"type" "text"
            "text" (:text block)}
    :image {"type"      "image_url"
            "image_url" {"url" (:url block)}}
    (throw (h/ex-info "Unsupported user content block for xAI"
                      {:block-type (:type block)}))))

(defn- user-content->wire
  [content]
  (if (string? content)
    content
    (mapv user-content-block->wire (model/content-blocks content))))

(defn- tool-call->wire
  [{:keys [id name args]}]
  {"id"       id
   "type"     "function"
   "function" {"name"      name
               "arguments" (json-encode args)}})

(defn- assistant-message->wire
  [message]
  (if-let [raw (-> message :provider-data :xai-message)]
    ;; replay content + tool_calls; reasoning_content must not be sent back
    (h/remove-empty-vals
     {"role"       "assistant"
      "content"    (get raw "content")
      "tool_calls" (get raw "tool_calls")})
    (let [blocks (model/content-blocks (:content message))]
      (h/remove-empty-vals
       {"role"       "assistant"
        "content"    (model/content-text (:content message))
        "tool_calls" (into []
                           (comp (filter #(= :tool-call (:type %)))
                                 (map tool-call->wire))
                           blocks)}))))

(defn- message->wire
  [{:keys [role content] :as message}]
  (case role
    :system    {"role"    "system"
                "content" (model/content-text content)}
    :user      {"role"    "user"
                "content" (user-content->wire content)}
    :assistant (assistant-message->wire message)
    :tool      {"role"         "tool"
                "tool_call_id" (:tool-call-id message)
                "content"      (str (model/content-text content))}
    (throw (h/ex-info "Unknown message role" {:role role}))))

(defn tool->wire
  [tool]
  (let [spec (or (:tool-specification tool) tool)]
    (cond
      (and (map? spec)
           (or (contains? spec :type) (contains? spec "type")))
      (stringify-keys-deep spec)

      (and (map? spec) (string? (:name spec)))
      {"type"     "function"
       "function" (h/remove-empty-vals
                   {"name"        (:name spec)
                    "description" (:description spec)
                    "parameters"  (:schema spec)})}

      :else
      (throw (h/ex-info
              "Cannot convert tool for the xAI API — use com.rpl.agent-o-rama.tools/tool (data specs)"
              {:tool-class (class spec)})))))

(defn- tool-choice->wire
  [tool-choice]
  (cond
    (keyword? tool-choice) (name tool-choice)
    (map? tool-choice)     {"type"     "function"
                            "function" {"name" (:name tool-choice)}}
    :else                  tool-choice))

(defn- output-schema->wire
  [output-schema]
  (let [{:keys [name schema strict?]
         :or   {name "response" strict? true}}
        (if (contains? output-schema :schema)
          output-schema
          {:schema output-schema})]
    {"type"        "json_schema"
     "json_schema" {"name"   name
                    "schema" schema
                    "strict" strict?}}))

(defn request->wire
  "Translates a neutral request map to an xAI chat-completions request body.
  defaults come from the model config; per-request keys win."
  [{default-model :model
    default-search :search-parameters
    default-effort :reasoning-effort}
   request]
  (let [search (if (contains? request :search-parameters)
                 (:search-parameters request)
                 default-search)
        effort (if (contains? request :reasoning-effort)
                 (:reasoning-effort request)
                 default-effort)]
    (h/remove-empty-vals
     {"model"             (or (:model request) default-model)
      "messages"          (mapv message->wire (:messages request))
      "tools"             (some->> (:tools request) (mapv tool->wire))
      "tool_choice"       (some-> (:tool-choice request) tool-choice->wire)
      "temperature"       (:temperature request)
      "top_p"             (:top-p request)
      "max_tokens"        (:max-output-tokens request)
      "stop"              (some-> (:stop-sequences request) vec)
      "response_format"   (some-> (:output-schema request)
                                  output-schema->wire)
      "search_parameters" (some-> search stringify-keys-deep)
      "reasoning_effort"  (some-> effort name)})))

;;; ---------------------------------------------------------------------------
;;; response translation: wire -> neutral
;;; ---------------------------------------------------------------------------

(defn- finish-reason->neutral
  [finish-reason has-tool-calls?]
  (cond
    has-tool-calls?                     :tool-calls
    (= "stop" finish-reason)            :stop
    (= "length" finish-reason)          :length
    (= "content_filter" finish-reason)  :refusal
    :else                               :other))

(defn response->neutral
  "Translates an xAI chat-completions response object (string-keyed map) to
  the neutral response format."
  [wire]
  (let [choice     (first (get wire "choices"))
        message    (get choice "message")
        content    (get message "content")
        reasoning  (get message "reasoning_content")
        tool-calls (into []
                         (map (fn [tc]
                                {:id   (get tc "id")
                                 :name (get-in tc ["function" "name"])
                                 :args (json-decode
                                        (get-in tc ["function" "arguments"]))}))
                         (get message "tool_calls"))
        blocks     (cond-> []
                     (seq reasoning)  (conj {:type    :reasoning
                                             :summary reasoning})
                     (seq content)    (conj {:type :text
                                             :text content})
                     (seq tool-calls) (into (map #(assoc % :type :tool-call)
                                                 tool-calls)))
        usage      (get wire "usage")]
    (h/remove-empty-vals
     {:message       {:role          :assistant
                      :content       blocks
                      :provider-data {:xai-message message}}
      :text          (when (seq content) content)
      :tool-calls    tool-calls
      :citations     (some-> (get wire "citations") vec)
      :finish-reason (finish-reason->neutral (get choice "finish_reason")
                                             (seq tool-calls))
      :usage         (h/remove-empty-vals
                      {:input-tokens     (get usage "prompt_tokens")
                       :output-tokens    (get usage "completion_tokens")
                       :total-tokens     (get usage "total_tokens")
                       :reasoning-tokens (get-in usage
                                                 ["completion_tokens_details"
                                                  "reasoning_tokens"])
                       :cached-input-tokens (get-in usage
                                                    ["prompt_tokens_details"
                                                     "cached_tokens"])})
      :model         (get wire "model")
      :response-id   (get wire "id")})))

(defn maybe-parse-output
  [response request]
  (if (and (:output-schema request) (:text response))
    (assoc response :parsed (json-decode (:text response)))
    response))

;;; ---------------------------------------------------------------------------
;;; SSE stream accumulation (chat-completions chunk format)
;;; ---------------------------------------------------------------------------

(defn process-sse-lines
  "Processes chat-completions SSE chunks, invoking on-event with neutral
  delta events, and accumulating a full response object (wire format, same
  shape as a non-streaming response). Throws on stream errors."
  [lines on-event]
  (let [state (volatile! {:content       nil
                          :reasoning     nil
                          :tool-calls    {}    ; index -> accumulated tool call
                          :finish-reason nil
                          :meta          nil   ; id/model/usage/citations
                          :error         nil})]
    (mhttp/each-sse-payload
     lines
     (fn [chunk]
       (if (get chunk "error")
         (vswap! state assoc :error (get chunk "error"))
         (let [choice (first (get chunk "choices"))
               delta  (get choice "delta")]
           (vswap! state
                   (fn [s]
                     (cond-> s
                       true
                       (update :meta
                               (fn [m]
                                 (-> (or m {})
                                     (update "id" #(or % (get chunk "id")))
                                     (update "model"
                                             #(or % (get chunk "model")))
                                     (update "usage"
                                             #(or (get chunk "usage") %))
                                     (update "citations"
                                             #(or (get chunk "citations")
                                                  %)))))
                       (get choice "finish_reason")
                       (assoc :finish-reason (get choice "finish_reason")))))
           (when-some [text (get delta "content")]
             (when (seq text)
               (on-event {:type :text-delta :text text})
               (vswap! state update :content str text)))
           (when-some [text (get delta "reasoning_content")]
             (when (seq text)
               (on-event {:type :reasoning-delta :text text})
               (vswap! state update :reasoning str text)))
           (doseq [tc (get delta "tool_calls")]
             (let [index (get tc "index" 0)]
               (when-some [args (get-in tc ["function" "arguments"])]
                 (when (seq args)
                   (on-event {:type :tool-call-delta :text args})))
               (vswap! state update-in [:tool-calls index]
                       (fn [acc]
                         (-> (or acc {"id"       nil
                                      "type"     "function"
                                      "function" {"name"      nil
                                                  "arguments" ""}})
                             (update "id" #(or % (get tc "id")))
                             (update-in ["function" "name"]
                                        #(or % (get-in tc
                                                       ["function" "name"])))
                             (update-in ["function" "arguments"]
                                        str
                                        (get-in tc
                                                ["function" "arguments"])))))))))))
    (let [{:keys [content reasoning tool-calls finish-reason meta error]}
          @state]
      (when error
        (throw (h/ex-info "xAI streaming request failed" {:error error})))
      (when-not meta
        (throw (h/ex-info "xAI stream ended without a response" {})))
      (merge
       (h/remove-empty-vals meta)
       {"choices" [{"finish_reason" finish-reason
                    "message"
                    (h/remove-empty-vals
                     {"role"              "assistant"
                      "content"           content
                      "reasoning_content" reasoning
                      "tool_calls"        (into []
                                                (map val)
                                                (sort-by key tool-calls))})}]}))))
