(ns com.rpl.agent-o-rama.model.openai
  "Native OpenAI integration for agent-o-rama, built on the Responses API
  (not the legacy Chat Completions API).

  Models created here satisfy com.rpl.agent-o-rama.model/ChatProvider, so
  declaring one as an agent object gives automatic trace recording and
  streaming with no further wiring. Use com.rpl.agent-o-rama.model/chat to
  call them.

  Because this adapter round-trips raw Responses API output items (including
  reasoning items with encrypted content) through the neutral message
  format, reasoning effort and tool calling work together in tool loops —
  conj the response's :message onto the history, append the tool results,
  and call chat again.

  Example:
  <pre>
  (openai/declare-model
   topology
   \"researcher\"
   {:api-key-env \"OPENAI_API_KEY\"
    :model       \"gpt-5.1\"
    :reasoning   {:effort :medium :summary :auto}})

  ;; in a node:
  (let [model (aor/get-agent-object agent-node \"researcher\")
        {:keys [message tool-calls text]}
        (model/chat model {:messages messages :tools TOOLS})]
    ...)
  </pre>"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.openai :as iopenai]
   [com.rpl.agent-o-rama.model :as model])
  (:import
   [java.io Closeable]))

(defn responses-model
  "Creates a ChatProvider backed by the OpenAI Responses API.

  opts:
    - :model - String model name, e.g. \"gpt-5.1\" (required)
    - :api-key - String API key; or
    - :api-key-env - Name of an environment variable holding the key
      (default: OPENAI_API_KEY is read when neither is given)
    - :reasoning - Default reasoning config for every request, e.g.
      {:effort :low|:medium|:high :summary :auto}; override per request
      with the :reasoning request key
    - :store? - Server-side response storage (default false; when false,
      encrypted reasoning content is requested automatically so reasoning
      round-trips statelessly through tool loops)
    - :stream? - Stream by default, auto-emitting text deltas via the
      agent's streaming machinery (default false); override per request
      with the :stream? request key
    - :base-url - API base (default \"https://api.openai.com/v1\")
    - :timeout-ms - Per-request timeout (default 600000)
    - :connect-timeout-ms - Connection timeout (default 10000)

  Request keys understood beyond the neutral ones: :reasoning, :store?,
  :include, :previous-response-id, :metadata."
  [{:keys [api-key api-key-env model base-url reasoning store? stream?
           timeout-ms]
    :as opts}]
  (when-not (string? model)
    (throw (h/ex-info "OpenAI model requires a :model string" {:opts opts})))
  (let [api-key  (or api-key
                     (some-> api-key-env (System/getenv))
                     (System/getenv "OPENAI_API_KEY"))
        _        (when-not (seq api-key)
                   (throw (h/ex-info "No OpenAI API key configured"
                                     {:hint ":api-key / :api-key-env / OPENAI_API_KEY"})))
        config   {:api-key    api-key
                  :base-url   (or base-url "https://api.openai.com/v1")
                  :timeout-ms (or timeout-ms 600000)
                  :client     (iopenai/mk-http-client opts)}
        defaults {:model     model
                  :reasoning reasoning
                  :store?    (if (contains? opts :store?) store? false)}]
    (reify
     model/ChatProvider
     (-provider-info [this]
       {:provider :openai
        :model    model
        :stream?  (boolean stream?)})
     (-chat [this request]
       (-> (iopenai/chat-http config (iopenai/request->wire defaults request))
           iopenai/response->neutral
           (iopenai/maybe-parse-output request)))
     (-stream-chat [this request on-delta]
       (-> (iopenai/stream-chat-http config
                                     (iopenai/request->wire defaults request)
                                     (or on-delta (fn [_])))
           iopenai/response->neutral
           (iopenai/maybe-parse-output request)))

     Closeable
     (close [this]
       (let [client (:client config)]
         (when (instance? java.lang.AutoCloseable client)
           (.close ^java.lang.AutoCloseable client)))))))

(defn declare-model
  "Declares a [[responses-model]] as an auto-traced agent object named
  object-name. Sugar for declare-agent-object-builder; use the builder form
  directly if the config needs other agent objects (e.g. a key held in one).

  <pre>
  (openai/declare-model topology \"researcher\"
    {:api-key-env \"OPENAI_API_KEY\"
     :model       \"gpt-5.1\"
     :reasoning   {:effort :medium}})
  </pre>"
  [topology object-name opts]
  (aor/declare-agent-object-builder
   topology
   object-name
   (fn [_setup] (responses-model opts))))

;;; built-in (server-side) tools — executed inside OpenAI, no client
;;; round-trip; results surface in the response as :built-in-tool-call blocks

(defn web-search
  "The web_search built-in tool, for a request's :tools.
  opts: :allowed-domains - collection of domains to restrict search to."
  ([] (web-search nil))
  ([{:keys [allowed-domains]}]
   (h/remove-empty-vals
    {"type"    "web_search"
     "filters" (when (seq allowed-domains)
                 {"allowed_domains" (vec allowed-domains)})})))

(defn code-interpreter
  "The code_interpreter built-in tool, for a request's :tools.
  opts: :container - container config (default {\"type\" \"auto\"})."
  ([] (code-interpreter nil))
  ([{:keys [container]}]
   {"type"      "code_interpreter"
    "container" (or container {"type" "auto"})}))
