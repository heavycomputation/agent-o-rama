(ns com.rpl.agent-o-rama.model.anthropic
  "Native Anthropic integration for agent-o-rama, built on the Messages API.

  Models created here satisfy com.rpl.agent-o-rama.model/ChatProvider, so
  declaring one as an agent object gives automatic trace recording and
  streaming. Use com.rpl.agent-o-rama.model/chat to call them.

  Extended thinking works across tool loops: adapter-produced assistant
  messages carry the raw content blocks (thinking blocks with signatures
  included) and replay them verbatim on the next turn, as the API requires.

  Example:
  <pre>
  (anthropic/declare-model
   topology
   \"claude\"
   {:api-key-env \"ANTHROPIC_API_KEY\"
    :model       \"claude-sonnet-5\"
    :max-tokens  4096
    :thinking    {:budget-tokens 8192}})
  </pre>"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.impl.anthropic :as ianthropic]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.model-http :as mhttp]
   [com.rpl.agent-o-rama.model :as model])
  (:import
   [java.io Closeable]))

(def ^:private ANTHROPIC-VERSION "2023-06-01")
(def ^:private STRUCTURED-OUTPUTS-BETA "structured-outputs-2025-11-13")

(defn- endpoint
  [{:keys [base-url api-key timeout-ms]} request]
  {:provider   :anthropic
   :url        (str base-url "/messages")
   :headers    (cond-> {"x-api-key"         api-key
                        "anthropic-version" ANTHROPIC-VERSION}
                 (:output-schema request)
                 (assoc "anthropic-beta" STRUCTURED-OUTPUTS-BETA))
   :timeout-ms timeout-ms})

(defn messages-model
  "Creates a ChatProvider backed by the Anthropic Messages API.

  opts:
    - :model - String model name, e.g. \"claude-sonnet-5\" (required)
    - :api-key - String API key; or
    - :api-key-env - Name of an environment variable holding the key
      (default: ANTHROPIC_API_KEY is read when neither is given)
    - :max-tokens - Default max_tokens, which the Messages API requires
      (default 4096); override per request with :max-output-tokens
    - :thinking - Default extended-thinking config, e.g.
      {:budget-tokens 8192}; override per request with the :thinking key
    - :stream? - Stream by default (default false); override per request
    - :base-url - API base (default \"https://api.anthropic.com/v1\")
    - :timeout-ms - Per-request timeout (default 600000)
    - :connect-timeout-ms - Connection timeout (default 10000)

  Request keys understood beyond the neutral ones: :thinking, :metadata.
  :output-schema uses Anthropic structured outputs (beta)."
  [{:keys [api-key api-key-env model base-url thinking max-tokens stream?
           timeout-ms]
    :as opts}]
  (when-not (string? model)
    (throw (h/ex-info "Anthropic model requires a :model string" {:opts opts})))
  (let [api-key  (or api-key
                     (some-> api-key-env (System/getenv))
                     (System/getenv "ANTHROPIC_API_KEY"))
        _        (when-not (seq api-key)
                   (throw (h/ex-info "No Anthropic API key configured"
                                     {:hint ":api-key / :api-key-env / ANTHROPIC_API_KEY"})))
        config   {:api-key    api-key
                  :base-url   (or base-url "https://api.anthropic.com/v1")
                  :timeout-ms (or timeout-ms 600000)
                  :client     (mhttp/mk-client opts)}
        defaults {:model      model
                  :thinking   thinking
                  :max-tokens max-tokens}]
    (reify
     model/ChatProvider
     (-provider-info [this]
       {:provider :anthropic
        :model    model
        :stream?  (boolean stream?)})
     (-chat [this request]
       (-> (mhttp/post-json (:client config)
                            (endpoint config request)
                            (ianthropic/request->wire defaults request))
           ianthropic/response->neutral
           (ianthropic/maybe-parse-output request)))
     (-stream-chat [this request on-delta]
       (let [on-delta (or on-delta (fn [_]))]
         (-> (mhttp/post-json-sse (:client config)
                                  (endpoint config request)
                                  (assoc (ianthropic/request->wire defaults
                                                                   request)
                                         "stream" true)
                                  #(ianthropic/process-sse-lines % on-delta))
             ianthropic/response->neutral
             (ianthropic/maybe-parse-output request))))

     Closeable
     (close [this]
       (mhttp/close-client! (:client config))))))

(defn declare-model
  "Declares a [[messages-model]] as an auto-traced agent object named
  object-name.

  <pre>
  (anthropic/declare-model topology \"claude\"
    {:model    \"claude-sonnet-5\"
     :thinking {:budget-tokens 8192}})
  </pre>"
  [topology object-name opts]
  (aor/declare-agent-object-builder
   topology
   object-name
   (fn [_setup] (messages-model opts))))

(defn web-search
  "The Anthropic web search server tool, for a request's :tools.
  opts: :max-uses - cap on searches per request
        :allowed-domains - collection of domains to restrict search to."
  ([] (web-search nil))
  ([{:keys [max-uses allowed-domains]}]
   (h/remove-empty-vals
    {"type"            "web_search_20250305"
     "name"            "web_search"
     "max_uses"        max-uses
     "allowed_domains" (some-> allowed-domains vec)})))
