;; ILLUSTRATIVE ONLY — signature-level sketch of the proposed lc4j-free API.
;; Multiple namespaces are shown in one file for readability; in the fork each
;; would live in its own file. Bodies are elided or pseudo-code.

;; ============================================================================
;; com.rpl.agent-o-rama.model — the neutral core
;; ============================================================================

(ns com.rpl.agent-o-rama.model
  "Provider-neutral chat interface. Everything is plain Clojure data:
   messages, tool calls, responses. Any object satisfying ChatProvider is
   automatically traced and streamed by agent-o-rama when declared as an
   agent object (this replaces the lc4j `instance? ChatModel` auto-wrapping
   in impl/agent_node.clj).")

(defprotocol ChatProvider
  "The single seam between agent-o-rama and an LLM provider.
   Implementations translate the neutral request map to the provider's
   native wire format (Responses API, Messages API, ...) and back."
  (chat* [this request]
    "Neutral request map -> neutral response map. Blocking (nodes run on
     virtual threads, same as today).")
  (stream-chat* [this request on-event]
    "Like chat*, but calls (on-event {:type :text-delta :text \"...\"}) /
     {:type :reasoning-delta ...} / {:type :tool-call-delta ...} as events
     arrive. Returns the complete neutral response map.")
  (trace-data [this]
    "Static model config for the trace UI: {\"provider\" \"openai\",
     \"model\" \"gpt-5.1\", \"reasoning\" {...}, ...}. Replaces the field
     plucking in record-model-call!."))

(defn chat
  "Send a chat request to any ChatProvider. THE function node code calls.

   request keys (all optional except :messages):
     :messages       - vector of message maps (see formats below)
     :tools          - tool definitions (from aor.tools/tool) and/or
                       provider built-ins (e.g. openai/web-search)
     :tool-choice    - :auto | :required | :none | {:name \"...\"}
     :output-schema  - JSON schema map (see aor.schema); response gains :parsed
     :max-output-tokens, :temperature, :top-p, :stop-sequences
     :reasoning      - OpenAI: {:effort :low|:medium|:high :summary :auto}
     :thinking       - Anthropic: {:budget-tokens 8192}
     :search-parameters - xAI Live Search config
     :previous-response-id - OpenAI Responses chaining
     (unknown keys pass through to the provider untouched)

   If the model was built with {:stream? true}, text deltas are automatically
   emitted via stream-chunk! on the current agent node — identical UX to
   today's StreamingChatModel auto-emission, no lc4j.

   Returns:
     {:message ..., :text ..., :tool-calls [...], :parsed ...,
      :finish-reason ..., :usage {...}, :provider {...}}"
  [model request]
  #_(instrumented (chat* model request)))

;; -- message constructors (sugar over plain maps) ---------------------------

(defn system    [text]      {:role :system :content text})
(defn user      [content]   {:role :user :content content})
(defn assistant [content]   {:role :assistant :content content})
(defn tool-result
  [tool-call result]
  {:role :tool
   :tool-call-id (:id tool-call)
   :name (:name tool-call)
   :content (str result)})

;; -- convenience graph builder ----------------------------------------------

(defn new-tool-loop-agent
  "Builds the standard model<->tools loop as a self-looping agent node, so
   each iteration is a separate traced node invoke (same shape every example
   currently hand-writes — see react.clj).

   options:
     :model-object - name of the declared ChatProvider agent object
     :tools-agent  - name of a tools agent (aor.tools/new-tools-agent)
     :tools        - tool definitions to offer the model
     :system       - optional system prompt prepended on first turn
     :max-turns    - loop bound (default 12)
     :request      - extra keys merged into every chat request"
  [topology agent-name options]
  #_...)

;; ============================================================================
;; com.rpl.agent-o-rama.model.openai — OpenAI Responses API (NOT Chat
;; Completions). Reasoning, built-in tools, response chaining, encrypted
;; reasoning persistence.
;; ============================================================================

(ns com.rpl.agent-o-rama.model.openai)

(defn responses-model
  "Returns a ChatProvider backed by POST /v1/responses.

   opts:
     :api-key    - string (or :api-key-fn for rotation)
     :model      - e.g. \"gpt-5.1\"
     :reasoning  - {:effort :low|:medium|:high :summary :auto} default for
                   every request (overridable per chat call)
     :stream?    - auto-emit text deltas via stream-chunk! (default false)
     :stream-include - #{:reasoning-summaries :tool-call-deltas} extras
     :store?     - server-side response storage (default false; when false the
                   adapter requests reasoning.encrypted_content so reasoning
                   items round-trip STATELESSLY through Rama depots — this is
                   what makes reasoning + tool loops work, and what lc4j
                   cannot express)
     :base-url, :timeout-ms, :max-retries"
  [opts]
  #_...)

(defn declare-model
  "Sugar: declares a responses-model as an auto-traced agent object.
   (openai/declare-model topology \"researcher\"
     {:api-key-env \"OPENAI_API_KEY\" :model \"gpt-5.1\"
      :reasoning {:effort :medium}})
   expands to declare-agent-object-builder + responses-model."
  [topology name opts]
  #_...)

;; Built-in (server-side) tools — run inside OpenAI, never round-trip to you.
;; They appear in traces as nested ops with citations/results.
(defn web-search       [& [{:keys [allowed-domains]}]] #_...)
(defn code-interpreter [& [opts]] #_...)
(defn file-search      [vector-store-ids & [opts]] #_...)

;; ============================================================================
;; com.rpl.agent-o-rama.model.anthropic — Anthropic Messages API with
;; extended thinking, server tools, prompt caching.
;; ============================================================================

(ns com.rpl.agent-o-rama.model.anthropic)

(defn messages-model
  "Returns a ChatProvider backed by POST /v1/messages.

   opts:
     :api-key, :model (e.g. \"claude-sonnet-5\"), :max-tokens
     :thinking  - {:budget-tokens 8192} — thinking blocks (with signatures)
                  are preserved in :message content and replayed verbatim on
                  the next turn, as the API requires for tool loops
     :stream?   - auto-emit text deltas (and optionally :thinking-deltas)
     :cache     - {:system true :tools true} — inject cache_control
                  breakpoints for prompt caching across loop iterations"
  [opts]
  #_...)

(defn declare-model [topology name opts] #_...)

;; Anthropic server tools
(defn web-search [& [opts]] #_...)

;; ============================================================================
;; com.rpl.agent-o-rama.model.xai — xAI Grok
;; ============================================================================

(ns com.rpl.agent-o-rama.model.xai)

(defn model
  "Returns a ChatProvider for xAI's API.

   opts:
     :api-key, :model (e.g. \"grok-4-1\"), :stream?
     :search-parameters - Live Search defaults, e.g.
                          {:mode :auto :sources [{:type :web} {:type :x}]}
                          citations surface in the response and the trace"
  [opts]
  #_...)

(defn declare-model [topology name opts] #_...)

;; ============================================================================
;; com.rpl.agent-o-rama.schema — plain-data JSON schema helpers
;; (replaces com.rpl.agent-o-rama.langchain4j.json; output is just maps)
;; ============================================================================

(ns com.rpl.agent-o-rama.schema)

(defn object  [props & [{:keys [description required]}]]
  #_{:type "object" :properties props :required required ...})
(defn string  [& [description]] #_...)
(defn number  [& [description]] #_...)
(defn integer [& [description]] #_...)
(defn boolean [& [description]] #_...)
(defn enum    [description values] #_...)
(defn array   [items & [description]] #_...)

;; ============================================================================
;; com.rpl.agent-o-rama.tools — same graph machinery, data-first definitions
;; ============================================================================

(ns com.rpl.agent-o-rama.tools)

(defn tool
  "Complete tool definition as data + fn. Replaces tool-specification +
   tool-info (no lc4j ToolSpecification).

   spec: {:name \"tavily\"
          :description \"Search the web\"
          :schema (s/object {\"terms\" (s/string \"...\")}
                            {:required [\"terms\"]})}
   f: (fn [args] ...) or (fn [agent-node caller-data args] ...) with
      {:include-context? true}"
  [spec f & [options]]
  #_...)

(defn new-tools-agent
  "Unchanged from today (begin -> tool -> agg-results fan-out graph), except:
   - input:  vector of tool-call maps {:id :name :args} (from :tool-calls)
   - output: vector of {:role :tool :tool-call-id :name :content} maps
   Error-handler options are unchanged."
  [topology name tools & [options]]
  #_...)
