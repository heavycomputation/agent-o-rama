(ns com.rpl.agent-o-rama.impl.openai
  "OpenAI Responses API integration internals: translation between the
  provider-neutral formats (com.rpl.agent-o-rama.model) and the Responses
  API wire format, plus HTTP/SSE transport over java.net.http.

  Key design points:
  - Assistant messages produced by this adapter carry the raw Responses API
    output items under [:provider-data :openai-output]. When such a message
    is replayed in a later request (e.g. a tool loop), the raw items —
    including reasoning items with encrypted content — are sent back
    verbatim, which is what lets reasoning models keep their chain of
    thought across tool calls with :store? false.
  - All wire-format maps are string-keyed; all neutral maps are
    keyword-keyed (except JSON-derived leaves like tool args, which keep
    string keys)."
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.model :as model]
   [jsonista.core :as j])
  (:import
   [java.net URI]
   [java.net.http
    HttpClient
    HttpRequest
    HttpRequest$BodyPublishers
    HttpResponse
    HttpResponse$BodyHandlers]
   [java.time Duration]
   [java.util Base64]
   [java.util.stream Stream]))

(def MAPPER (j/object-mapper {:decode-key-fn str}))

(defn- json-encode
  ^String [x]
  (j/write-value-as-string x))

(defn- json-decode
  [^String s]
  (j/read-value s MAPPER))

;;; ---------------------------------------------------------------------------
;;; request translation: neutral -> wire
;;; ---------------------------------------------------------------------------

(defn- data-url
  [{:keys [mime-type data]}]
  (str "data:" (or mime-type "application/octet-stream")
       ";base64," (.encodeToString (Base64/getEncoder) ^bytes data)))

(defn- user-content-block->wire
  [block]
  (case (:type block)
    :text  {"type" "input_text"
            "text" (:text block)}
    :image {"type"      "input_image"
            "image_url" (or (:url block)
                            (when (:data block) (data-url block)))}
    (throw (h/ex-info "Unsupported user content block for OpenAI"
                      {:block-type (:type block)}))))

(defn- user-content->wire
  [content]
  (if (string? content)
    content
    (mapv user-content-block->wire (model/content-blocks content))))

(defn- assistant-message->wire-items
  [message]
  (if-let [raw-items (-> message :provider-data :openai-output)]
    ;; adapter-produced message: replay the raw output items verbatim
    ;; (reasoning items, function calls, and all)
    (vec raw-items)
    ;; hand-constructed assistant message
    (into []
          (keep (fn [block]
                  (case (:type block)
                    :text      {"type"    "message"
                                "role"    "assistant"
                                "content" [{"type"        "output_text"
                                            "text"        (:text block)
                                            "annotations" []}]}
                    :tool-call {"type"      "function_call"
                                "call_id"   (:id block)
                                "name"      (:name block)
                                "arguments" (json-encode (:args block))}
                    ;; reasoning can't be reconstructed without provider data
                    nil)))
          (model/content-blocks (:content message)))))

(defn- message->wire-items
  [{:keys [role content] :as message}]
  (case role
    :system    [{"role"    "system"
                 "content" (model/content-text content)}]
    :user      [{"role"    "user"
                 "content" (user-content->wire content)}]
    :assistant (assistant-message->wire-items message)
    :tool      [{"type"    "function_call_output"
                 "call_id" (:tool-call-id message)
                 "output"  (str (model/content-text content))}]
    (throw (h/ex-info "Unknown message role" {:role role}))))

(defn messages->input
  [messages]
  (into [] (mapcat message->wire-items) messages))

(defn- stringify-keys
  [m]
  (into {}
        (map (fn [[k v]]
               [(if (keyword? k) (name k) k)
                (if (map? v) (stringify-keys v) v)]))
        m))

(defn tool->wire
  "Converts a tool given in a request's :tools to the wire format. Accepts:
  - ToolInfo records / data spec maps from com.rpl.agent-o-rama.tools/tool
  - provider built-in tool maps ({:type \"web_search\"} et al), passed through"
  [tool]
  (let [spec (or (:tool-specification tool) tool)]
    (cond
      (and (map? spec)
           (or (contains? spec :type) (contains? spec "type")))
      (stringify-keys spec)

      (and (map? spec) (string? (:name spec)))
      (h/remove-empty-vals
       {"type"        "function"
        "name"        (:name spec)
        "description" (:description spec)
        "parameters"  (:schema spec)
        "strict"      (:strict? spec)})

      :else
      (throw (h/ex-info
              "Cannot convert tool for the OpenAI Responses API — use com.rpl.agent-o-rama.tools/tool (data specs), not langchain4j tool-specification"
              {:tool-class (class spec)})))))

(defn- tool-choice->wire
  [tool-choice]
  (cond
    (keyword? tool-choice) (name tool-choice)
    (map? tool-choice)     {"type" "function"
                            "name" (:name tool-choice)}
    :else                  tool-choice))

(defn- output-schema->wire
  [output-schema]
  (let [{:keys [name schema strict?]
         :or   {name "response" strict? true}}
        (if (contains? output-schema :schema)
          output-schema
          {:schema output-schema})]
    {"format" {"type"   "json_schema"
               "name"   name
               "schema" schema
               "strict" strict?}}))

(defn- reasoning->wire
  [reasoning]
  (h/remove-empty-vals
   {"effort"  (some-> (:effort reasoning) name)
    "summary" (some-> (:summary reasoning) name)}))

(defn request->wire
  "Translates a neutral request map to a Responses API request body
  (string-keyed, ready for JSON encoding). defaults come from the model
  config; per-request keys win."
  [{default-model :model
    default-reasoning :reasoning
    default-store? :store?
    :as defaults}
   request]
  (let [reasoning (if (contains? request :reasoning)
                    (:reasoning request)
                    default-reasoning)
        store?    (if (contains? request :store?)
                    (:store? request)
                    default-store?)
        include   (cond-> (vec (:include request []))
                    (and (false? store?)
                         (not-any? #{"reasoning.encrypted_content"}
                                   (:include request [])))
                    (conj "reasoning.encrypted_content"))]
    (cond->
     (h/remove-empty-vals
      {"model"                (or (:model request) default-model)
       "input"                (messages->input (:messages request))
       "tools"                (some->> (:tools request) (mapv tool->wire))
       "tool_choice"          (some-> (:tool-choice request) tool-choice->wire)
       "temperature"          (:temperature request)
       "top_p"                (:top-p request)
       "max_output_tokens"    (:max-output-tokens request)
       "previous_response_id" (:previous-response-id request)
       "reasoning"            (some-> reasoning reasoning->wire)
       "text"                 (some-> (:output-schema request)
                                      output-schema->wire)
       "metadata"             (:metadata request)})
      (some? store?) (assoc "store" store?)
      (seq include)  (assoc "include" include))))

;;; ---------------------------------------------------------------------------
;;; response translation: wire -> neutral
;;; ---------------------------------------------------------------------------

(defn- reasoning-summary-text
  [reasoning-item]
  (let [texts (keep #(get % "text") (get reasoning-item "summary"))]
    (when (seq texts)
      (str/join "\n\n" texts))))

(defn- output-item->blocks
  [item]
  (case (get item "type")
    "reasoning"
    [(h/remove-empty-vals
      {:type    :reasoning
       :summary (reasoning-summary-text item)})]

    "message"
    (into []
          (map (fn [content-item]
                 (case (get content-item "type")
                   "output_text" {:type :text
                                  :text (get content-item "text")}
                   "refusal"     {:type :refusal
                                  :text (get content-item "refusal")}
                   {:type :unknown
                    :item content-item})))
          (get item "content"))

    "function_call"
    [{:type :tool-call
      :id   (get item "call_id")
      :name (get item "name")
      :args (json-decode (get item "arguments"))}]

    ;; server-side tool executions (web_search_call, code_interpreter_call,
    ;; ...) and anything future: surface as an opaque block
    [{:type :built-in-tool-call
      :item item}]))

(defn response->neutral
  "Translates a Responses API response object (string-keyed map) to the
  neutral response format."
  [wire]
  (let [items      (vec (get wire "output"))
        blocks     (into [] (mapcat output-item->blocks) items)
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
        incomplete-reason (get-in wire ["incomplete_details" "reason"])
        finish     (cond
                     (some #(= :refusal (:type %)) blocks)   :refusal
                     (= "max_output_tokens" incomplete-reason) :length
                     (= "incomplete" (get wire "status"))      :other
                     (seq tool-calls)                          :tool-calls
                     :else                                     :stop)]
    (h/remove-empty-vals
     {:message       {:role          :assistant
                      :content       blocks
                      :provider-data {:openai-output items}}
      :text          text
      :tool-calls    tool-calls
      :finish-reason finish
      :usage         (h/remove-empty-vals
                      {:input-tokens        (get usage "input_tokens")
                       :output-tokens       (get usage "output_tokens")
                       :total-tokens        (get usage "total_tokens")
                       :reasoning-tokens    (get-in usage
                                                    ["output_tokens_details"
                                                     "reasoning_tokens"])
                       :cached-input-tokens (get-in usage
                                                    ["input_tokens_details"
                                                     "cached_tokens"])})
      :model         (get wire "model")
      :response-id   (get wire "id")})))

(defn maybe-parse-output
  "When the request asked for structured output, adds :parsed (the response
  text decoded as JSON, string keys)."
  [response request]
  (if (and (:output-schema request) (:text response))
    (assoc response :parsed (json-decode (:text response)))
    response))

;;; ---------------------------------------------------------------------------
;;; SSE event processing (pure — lines in, final response object out)
;;; ---------------------------------------------------------------------------

(defn process-sse-lines
  "Processes a seq of SSE lines from a Responses API streaming request,
  invoking on-event with neutral delta events as they occur. Returns the
  final complete response object (wire format). Throws on stream errors."
  [lines on-event]
  (let [final (volatile! nil)
        error (volatile! nil)]
    (doseq [^String line lines]
      (when (str/starts-with? line "data:")
        (let [payload (str/trim (subs line 5))]
          (when-not (= "[DONE]" payload)
            (let [event      (json-decode payload)
                  event-type (get event "type")]
              (case event-type
                "response.output_text.delta"
                (on-event {:type :text-delta
                           :text (get event "delta")})

                "response.reasoning_summary_text.delta"
                (on-event {:type :reasoning-delta
                           :text (get event "delta")})

                "response.function_call_arguments.delta"
                (on-event {:type :tool-call-delta
                           :text (get event "delta")})

                "response.refusal.delta"
                (on-event {:type :refusal-delta
                           :text (get event "delta")})

                ("response.completed" "response.incomplete")
                (vreset! final (get event "response"))

                "response.failed"
                (vreset! error (or (get-in event ["response" "error"])
                                   event))

                "error"
                (vreset! error event)

                nil))))))
    (when @error
      (throw (h/ex-info "OpenAI streaming request failed" {:error @error})))
    (when-not @final
      (throw (h/ex-info "OpenAI stream ended without a complete response" {})))
    @final))

;;; ---------------------------------------------------------------------------
;;; HTTP transport
;;; ---------------------------------------------------------------------------

(defn- http-request
  ^HttpRequest [{:keys [base-url api-key timeout-ms]} body]
  (-> (HttpRequest/newBuilder (URI/create (str base-url "/responses")))
      (.timeout (Duration/ofMillis timeout-ms))
      (.header "Authorization" (str "Bearer " api-key))
      (.header "Content-Type" "application/json")
      (.POST (HttpRequest$BodyPublishers/ofString (json-encode body)))
      .build))

(defn- request-failed
  [status body]
  (h/ex-info "OpenAI request failed"
             {:status status
              :error  (try
                        (get (json-decode body) "error")
                        (catch Exception _ body))}))

(defn chat-http
  "Synchronous Responses API call. Returns the response object (wire format)."
  [{:keys [^HttpClient client] :as config} wire-request]
  (let [^HttpResponse resp (.send client
                                  (http-request config wire-request)
                                  (HttpResponse$BodyHandlers/ofString))]
    (if (<= 200 (.statusCode resp) 299)
      (json-decode (.body resp))
      (throw (request-failed (.statusCode resp) (.body resp))))))

(defn stream-chat-http
  "Streaming Responses API call. Invokes on-event with neutral delta events
  as SSE events arrive; returns the final response object (wire format)."
  [{:keys [^HttpClient client] :as config} wire-request on-event]
  (let [^HttpResponse resp (.send client
                                  (http-request config
                                                (assoc wire-request
                                                       "stream" true))
                                  (HttpResponse$BodyHandlers/ofLines))
        lines (iterator-seq (.iterator ^Stream (.body resp)))]
    (if (<= 200 (.statusCode resp) 299)
      (process-sse-lines lines on-event)
      (throw (request-failed (.statusCode resp)
                             (str/join "\n" lines))))))

(defn mk-http-client
  ^HttpClient [{:keys [connect-timeout-ms]
                :or   {connect-timeout-ms 10000}}]
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofMillis connect-timeout-ms))
      .build))
