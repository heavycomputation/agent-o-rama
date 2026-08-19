(ns com.rpl.agent-o-rama.model.xai
  "Native xAI (Grok) integration for agent-o-rama.

  Models created here satisfy com.rpl.agent-o-rama.model/ChatProvider, so
  declaring one as an agent object gives automatic trace recording and
  streaming. Use com.rpl.agent-o-rama.model/chat to call them.

  xAI extensions supported:
  - Live Search via :search-parameters (server-side; citations surface under
    the response's :citations key)
  - reasoning models: reasoning_content comes back as a :reasoning block and
    reasoning token counts land in :usage; :reasoning-effort is passed for
    models that accept it

  Example:
  <pre>
  (xai/declare-model
   topology
   \"grok\"
   {:api-key-env       \"XAI_API_KEY\"
    :model             \"grok-4-1\"
    :search-parameters {:mode \"auto\"}})
  </pre>"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.model-http :as mhttp]
   [com.rpl.agent-o-rama.impl.xai :as ixai]
   [com.rpl.agent-o-rama.model :as m])
  (:import
   [java.io Closeable]))

(defn- endpoint
  [{:keys [base-url api-key timeout-ms]}]
  {:provider   :xai
   :url        (str base-url "/chat/completions")
   :headers    {"Authorization" (str "Bearer " api-key)}
   :timeout-ms timeout-ms})

(defn model
  "Creates a ChatProvider backed by the xAI API.

  opts:
    - :model - String model name, e.g. \"grok-4-1\" (required)
    - :api-key - String API key; or
    - :api-key-env - Name of an environment variable holding the key
      (default: XAI_API_KEY is read when neither is given)
    - :search-parameters - Default Live Search config, e.g.
      {:mode \"auto\" :sources [{:type \"web\"} {:type \"x\"}]}; override
      per request with the :search-parameters key
    - :reasoning-effort - Default reasoning effort for models that accept
      it (:low | :high); override per request
    - :stream? - Stream by default (default false); override per request
    - :base-url - API base (default \"https://api.x.ai/v1\")
    - :timeout-ms - Per-request timeout (default 600000)
    - :connect-timeout-ms - Connection timeout (default 10000)"
  [{:keys [api-key api-key-env model base-url search-parameters
           reasoning-effort stream? timeout-ms]
    :as opts}]
  (when-not (string? model)
    (throw (h/ex-info "xAI model requires a :model string" {:opts opts})))
  (let [api-key  (or api-key
                     (some-> api-key-env (System/getenv))
                     (System/getenv "XAI_API_KEY"))
        _        (when-not (seq api-key)
                   (throw (h/ex-info "No xAI API key configured"
                                     {:hint ":api-key / :api-key-env / XAI_API_KEY"})))
        config   {:api-key    api-key
                  :base-url   (or base-url "https://api.x.ai/v1")
                  :timeout-ms (or timeout-ms 600000)
                  :client     (mhttp/mk-client opts)}
        defaults {:model             model
                  :search-parameters search-parameters
                  :reasoning-effort  reasoning-effort}]
    (reify
     m/ChatProvider
     (-provider-info [this]
       {:provider :xai
        :model    model
        :stream?  (boolean stream?)})
     (-chat [this request]
       (-> (mhttp/post-json (:client config)
                            (endpoint config)
                            (ixai/request->wire defaults request))
           ixai/response->neutral
           (ixai/maybe-parse-output request)))
     (-stream-chat [this request on-delta]
       (let [on-delta (or on-delta (fn [_]))]
         (-> (mhttp/post-json-sse (:client config)
                                  (endpoint config)
                                  (assoc (ixai/request->wire defaults request)
                                         "stream" true)
                                  #(ixai/process-sse-lines % on-delta))
             ixai/response->neutral
             (ixai/maybe-parse-output request))))

     Closeable
     (close [this]
       (mhttp/close-client! (:client config))))))

(defn declare-model
  "Declares a [[model]] as an auto-traced agent object named object-name.

  <pre>
  (xai/declare-model topology \"grok\" {:model \"grok-4-1\"})
  </pre>"
  [topology object-name opts]
  (aor/declare-agent-object-builder
   topology
   object-name
   (fn [_setup] (model opts))))
