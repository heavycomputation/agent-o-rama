(ns com.rpl.agent-o-rama.model
  "Provider-neutral chat model interface for agent-o-rama.

  This namespace defines the seam between agent-o-rama and LLM providers.
  Any object satisfying [[ChatProvider]] that is declared as an agent object
  (via declare-agent-object-builder) is automatically instrumented: model
  calls are recorded in the trace UI, streaming deltas are emitted through
  the agent's streaming machinery (consumable with agent-stream), and
  time-to-first-token and token usage are captured.

  All data crossing this interface is plain Clojure data, so it serializes
  through Rama depots/PStates with no custom codecs.

  ## Message format

  A message is a map with a :role of :system, :user, :assistant, or :tool:

    {:role :system :content \"You are terse.\"}
    {:role :user :content \"hi\"}                    ; string content sugar
    {:role :user :content [{:type :text :text \"hi\"}]}
    {:role :assistant
     :content [{:type :reasoning :summary \"...\" :provider-data {...}}
               {:type :text :text \"...\"}
               {:type :tool-call :id \"c1\" :name \"add\" :args {\"a\" 1}}]}
    {:role :tool :tool-call-id \"c1\" :name \"add\" :content \"3\"}

  Content is either a string or a vector of content blocks. Standard block
  types are :text, :reasoning, :tool-call, :image, :audio, :pdf; providers
  may round-trip additional types (e.g. opaque reasoning payloads in
  :provider-data) and consumers must tolerate unknown types.

  ## Request format

  A request is a map; :messages is required, everything else is optional
  and passed through to the provider (providers ignore keys they don't
  understand):

    {:messages [...]            ; vector of messages (strings become :user)
     :tools [...]               ; tool definitions and/or provider built-ins
     :tool-choice :auto         ; :auto | :required | :none | {:name \"..\"}
     :output-schema {...}       ; JSON schema map; response gains :parsed
     :model \"...\"             ; override the provider's default model
     :max-output-tokens 500
     :temperature 0.2
     :top-p 1.0 :top-k nil :stop-sequences [...]
     :stream? true}             ; per-request override of the provider default

  ## Response format

    {:message {...}             ; the assistant message, ready to conj onto
                                ; the message history (reasoning/tool-call
                                ; blocks included, so tool loops round-trip
                                ; correctly)
     :text \"...\"              ; concatenated text content, or nil
     :tool-calls [{:id \"c1\" :name \"add\" :args {\"a\" 1}}]
     :parsed {...}              ; decoded structured output when
                                ; :output-schema was given
     :finish-reason :stop       ; :stop | :tool-calls | :length | :refusal
                                ; | :content-filter | :other
     :usage {:input-tokens 7 :output-tokens 5 :total-tokens 12
             :reasoning-tokens 0 :cached-input-tokens 0}
     :model \"...\"             ; concrete model that served the request
     :response-id \"...\"}      ; provider response id, when available

  ## Streaming delta format

  Providers report streaming events as plain strings (text deltas) or maps:

    {:type :text-delta :text \"...\"}
    {:type :reasoning-delta :text \"...\"}
    {:type :tool-call-delta ...}

  Text deltas are automatically forwarded to stream-chunk! on the current
  agent node; other event types are available to a caller-supplied handler."
  (:require
   [com.rpl.agent-o-rama.impl.helpers :as h]))

(defprotocol ChatProvider
  "SPI implemented by LLM provider integrations (OpenAI, Anthropic, xAI,
  or an adapter over another client library). Application code should call
  [[chat]] rather than these methods directly — agent-o-rama wraps declared
  agent objects satisfying this protocol in an instrumented proxy whose
  method implementations record traces and emit streaming chunks."
  (-chat [provider request]
    "Neutral request map -> neutral response map. Blocking (agent nodes run
    on virtual threads).")
  (-stream-chat [provider request on-delta]
    "Like -chat, but must invoke (on-delta event) for each streaming event
    as it arrives (see the namespace docs for the event format) and return
    the complete neutral response map.")
  (-provider-info [provider]
    "Static configuration for instrumentation and dispatch:
    {:provider :openai        ; provider name keyword
     :model \"gpt-5.1\"       ; default model
     :stream? false}          ; whether chat calls stream by default
    plus any other config the provider wants surfaced in traces."))

;;; message constructors — sugar over plain maps

(defn system
  "Creates a system message: {:role :system :content text}"
  [text]
  {:role :system :content text})

(defn user
  "Creates a user message. content is a string or a vector of content
  blocks (see namespace docs)."
  [content]
  {:role :user :content content})

(defn assistant
  "Creates an assistant message. content is a string or a vector of content
  blocks. Responses from [[chat]] already contain a complete assistant
  message under :message; this is mainly for constructing histories by hand."
  [content]
  {:role :assistant :content content})

(defn tool-result
  "Creates a tool result message answering the given tool call
  ({:id ... :name ...}, as found in a response's :tool-calls).
  Tools agents (com.rpl.agent-o-rama.tools) produce these automatically."
  [tool-call result]
  {:role         :tool
   :tool-call-id (:id tool-call)
   :name         (:name tool-call)
   :content      (str result)})

(defn content-blocks
  "Normalizes message content to a vector of content block maps. String
  content becomes a single :text block."
  [content]
  (cond
    (nil? content)     []
    (string? content)  [{:type :text :text content}]
    (sequential? content) (vec content)
    :else (throw (h/ex-info "Invalid message content"
                            {:type (class content)}))))

(defn content-text
  "Extracts the concatenated text of a message's content, or nil if it has
  no :text blocks."
  [content]
  (let [texts (into []
                    (comp (filter #(= :text (:type %)))
                          (keep :text))
                    (content-blocks content))]
    (when (seq texts)
      (apply str texts))))

(defn delta-text
  "Returns the text of a streaming delta event, or nil for non-text events.
  Plain strings are treated as text deltas."
  [delta]
  (cond
    (string? delta) delta
    (and (map? delta) (= :text-delta (:type delta))) (:text delta)
    :else nil))

;;; request normalization + entry point

(defn- normalize-message
  [m]
  (cond
    (string? m) (user m)
    (and (map? m) (keyword? (:role m))) m
    :else (throw (h/ex-info "Invalid message" {:message m}))))

(defn- normalize-request
  [request]
  (cond
    (string? request)
    {:messages [(user request)]}

    (sequential? request)
    {:messages (mapv normalize-message request)}

    (map? request)
    (do
      (when-not (sequential? (:messages request))
        (throw (h/ex-info "Request map must contain a :messages vector"
                          {:keys (keys request)})))
      (update request :messages (partial mapv normalize-message)))

    :else
    (throw (h/ex-info "Unknown request type" {:type (class request)}))))

(defn- streaming-request?
  [provider request]
  (if (contains? request :stream?)
    (boolean (:stream? request))
    (boolean (:stream? (-provider-info provider)))))

(defn chat
  "Sends a chat request to a [[ChatProvider]] and returns the neutral
  response map (see namespace docs for both formats).

  request can be:
    - a request map {:messages [...] ...}
    - a vector of messages (strings become :user messages)
    - a string prompt

  Whether the call streams is decided by the request's :stream? key when
  present, otherwise by the provider's default. On a streaming call, text
  deltas are automatically emitted via stream-chunk! on the current agent
  node; pass on-delta to additionally receive every streaming event
  (including non-text events) yourself.

  Example:
  <pre>
  (let [model (aor/get-agent-object agent-node \"openai\")
        {:keys [message tool-calls text]}
        (model/chat model {:messages messages :tools TOOLS})]
    ...)
  </pre>"
  ([provider request]
   (chat provider request nil))
  ([provider request on-delta]
   (when-not (satisfies? ChatProvider provider)
     (throw (h/ex-info
             "Object does not satisfy com.rpl.agent-o-rama.model/ChatProvider"
             {:type (class provider)})))
   (let [request (normalize-request request)]
     (if (or on-delta (streaming-request? provider request))
       (-stream-chat provider request on-delta)
       (-chat provider request)))))
