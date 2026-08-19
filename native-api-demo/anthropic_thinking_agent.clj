(ns com.rpl.agent.native-demo.anthropic-thinking-agent
  "ILLUSTRATIVE ONLY — does not compile. Sketches a customer-support triage
  agent on the hypothetical native Anthropic integration (Messages API).

  What this shows:
  - Extended thinking + tool use in a loop. Thinking blocks (with their
    signatures) live inside the assistant message map and are replayed
    verbatim on the next turn, as the Messages API requires. Plain data in,
    plain data out — they survive Rama depots with zero custom serializers.
  - Streaming: the model is built with :stream? true, so text deltas are
    auto-emitted through the existing stream-chunk! machinery and consumed
    with the unchanged aor/agent-stream client API.
  - Prompt caching across loop iterations via :cache breakpoints."
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.anthropic :as anthropic]
   [com.rpl.agent-o-rama.schema :as s]
   [com.rpl.agent-o-rama.tools :as t]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

(def lookup-order-tool
  (t/tool
   {:name        "lookup_order"
    :description "Look up an order by id"
    :schema      (s/object {"order_id" (s/string "The order id")}
                           {:required ["order_id"]})}
   (fn [agent-node _ {:strs [order_id]}]
     (let [orders (aor/get-store agent-node "$$orders")]
       (or (aor/get orders order_id) "No such order")))
   {:include-context? true}))

(def escalate-tool
  (t/tool
   {:name        "escalate"
    :description "Escalate to a human support agent"
    :schema      (s/object {"reason" (s/string "Why this needs a human")}
                           {:required ["reason"]})}
   (fn [{:strs [reason]}]
     (str "Escalated: " reason))))

(def TOOLS [lookup-order-tool escalate-tool])

(aor/defagentmodule AnthropicSupportModule
  [topology]

  (anthropic/declare-model
   topology
   "claude"
   {:api-key    (System/getenv "ANTHROPIC_API_KEY")
    :model      "claude-sonnet-5"
    :max-tokens 4096
    ;; extended thinking: the model reasons before responding/choosing tools;
    ;; thinking blocks show up in the trace UI as part of the ai message
    :thinking   {:budget-tokens 8192}
    ;; auto-emit text deltas via stream-chunk!; thinking deltas opt-in
    :stream?    true
    ;; cache_control breakpoints on system + tools: iterations 2..n of the
    ;; tool loop hit the prompt cache instead of re-billing the prefix
    :cache      {:system true :tools true}})

  (t/new-tools-agent topology "tools" TOOLS)

  (->
    topology
    (aor/new-agent "SupportAgent")
    (aor/node
     "triage"
     "triage"
     (fn [agent-node messages]
       (let [claude (aor/get-agent-object agent-node "claude")
             tools  (aor/agent-client agent-node "tools")
             ;; tokens stream to subscribers while this call blocks;
             ;; :message content is [{:type :thinking ...} {:type :text ...}
             ;;                      {:type :tool-call ...}]
             {:keys [message tool-calls text finish-reason]}
             (m/chat claude {:messages messages :tools TOOLS})]
         (cond
           (seq tool-calls)
           (let [tool-results (aor/agent-invoke tools tool-calls)]
             ;; conj'ing :message preserves the thinking blocks + signatures
             ;; the API requires on replay — automatic, not glue code
             (aor/emit! agent-node "triage"
                        (into (conj messages message) tool-results)))

           (= :refusal finish-reason)
           (aor/result! agent-node {:status :refused :reply text})

           :else
           (aor/result! agent-node {:status :answered :reply text})))))))

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              _   (aor/start-ui ipc)]
    (rtest/launch-module! ipc AnthropicSupportModule {:tasks 4 :threads 2})
    (let [manager (aor/agent-manager ipc (rama/get-module-name AnthropicSupportModule))
          agent   (aor/agent-client manager "SupportAgent")
          invoke  (aor/agent-initiate
                   agent
                   [(m/system "You are a support agent for Acme. Use tools; escalate when unsure.")
                    (m/user "My order AC-1042 arrived damaged, what are my options?")])]
      ;; unchanged client streaming API — chunks here are the text deltas the
      ;; Anthropic adapter fed to stream-chunk! inside the node
      (aor/agent-stream
       agent
       invoke
       "triage"
       (fn [_all new-chunks _reset? _complete?]
         (doseq [chunk new-chunks] (print chunk) (flush))))
      (aor/agent-result agent invoke))))
