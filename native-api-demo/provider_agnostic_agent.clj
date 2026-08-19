(ns com.rpl.agent.native-demo.provider-agnostic-agent
  "ILLUSTRATIVE ONLY — does not compile. The payoff of the neutral layer:

  1. The SAME node code runs against OpenAI, Anthropic, and xAI. Because
     m/chat takes and returns identical plain-data shapes for every
     provider, swapping providers is a one-line agent-object change —
     useful directly with AOR's comparative experiments to A/B providers
     on a dataset.

  2. The hand-written model<->tools loop (which every current example
     copies) collapses into one declaration: m/new-tool-loop-agent."
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.anthropic :as anthropic]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.model.xai :as xai]
   [com.rpl.agent-o-rama.schema :as s]
   [com.rpl.agent-o-rama.tools :as t]))

(def weather-tool
  (t/tool
   {:name        "get_weather"
    :description "Current weather for a city"
    :schema      (s/object {"city" (s/string "City name")}
                           {:required ["city"]})}
   (fn [{:strs [city]}]
     (str "22C and sunny in " city))))

(def TOOLS [weather-tool])

(aor/defagentmodule ProviderAgnosticModule
  [topology]

  ;; Three providers, three declarations, identical downstream usage.
  ;; Provider-specific capabilities live in the model config, not node code.
  (openai/declare-model    topology "gpt"
    {:api-key-env "OPENAI_API_KEY"
     :model       "gpt-5.1"
     :reasoning   {:effort :low}})
  (anthropic/declare-model topology "claude"
    {:api-key-env "ANTHROPIC_API_KEY"
     :model       "claude-sonnet-5"
     :max-tokens  2048})
  (xai/declare-model       topology "grok"
    {:api-key-env "XAI_API_KEY"
     :model       "grok-4-1"})

  (t/new-tools-agent topology "tools" TOOLS)

  ;; ---- 1. provider chosen per-invoke; node body is provider-blind --------
  (->
    topology
    (aor/new-agent "AskAnyProvider")
    (aor/node
     "chat"
     "chat"
     (fn [agent-node {:keys [model-object messages] :as state}]
       (let [model (aor/get-agent-object agent-node model-object)
             tools (aor/agent-client agent-node "tools")
             {:keys [message tool-calls text]}
             (m/chat model {:messages messages :tools TOOLS})]
         (if (seq tool-calls)
           (aor/emit! agent-node "chat"
                      (assoc state :messages
                             (into (conj messages message)
                                   (aor/agent-invoke tools tool-calls))))
           (aor/result! agent-node text))))))
  ;; invoke with {:model-object "claude" :messages [(m/user "...")]} — or
  ;; "gpt" or "grok". Same code path, same traces, same streaming.

  ;; ---- 2. the whole loop above as one declaration -------------------------
  (m/new-tool-loop-agent
   topology
   "WeatherAssistant"
   {:model-object "gpt"
    :tools-agent  "tools"
    :tools        TOOLS
    :system       "You are a concise weather assistant."
    :max-turns    6}))

;; Comparative experiment sketch: because the agent input is data and the
;; provider is an agent object name, AOR's existing experiments UI can run
;; the same dataset against "gpt" vs "claude" vs "grok" and score outputs
;; side by side — no new machinery needed.
