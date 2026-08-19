(ns com.rpl.agent.native-demo.openai-research-agent
  "ILLUSTRATIVE ONLY — does not compile. Sketches a research agent on the
  hypothetical native OpenAI integration (Responses API).

  What this shows that the lc4j-based version cannot do:
  - Reasoning effort + tool calling in the SAME loop. The assistant message
    returned by m/chat carries the reasoning items (encrypted, opaque), and
    because messages are plain data they flow through Rama depots and back
    to OpenAI intact on the next turn. lc4j's ChatModel drops them.
  - A server-side built-in tool (web_search) alongside custom function tools.
  - Reasoning-token usage and reasoning summaries land in the trace UI
    automatically via the neutral trace format."
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as s]
   [com.rpl.agent-o-rama.tools :as t]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; Custom function tool — pure data spec + Clojure fn. No ToolSpecification,
;;; no lc4j json builders.
(def save-note-tool
  (t/tool
   {:name        "save_note"
    :description "Persist a research note for the final report"
    :schema      (s/object
                  {"topic" (s/string "Topic the note belongs to")
                   "note"  (s/string "The note text")}
                  {:required ["topic" "note"]})}
   (fn [agent-node _caller-data {:strs [topic note]}]
     (let [notes (aor/get-store agent-node "$$notes")]
       (aor/put! notes topic note)
       "saved"))
   {:include-context? true}))

(def TOOLS [save-note-tool])

(aor/defagentmodule OpenAIResearchModule
  [topology]

  ;; One call replaces api-key object + declare-agent-object-builder +
  ;; OpenAiChatModel/builder interop. The returned object satisfies
  ;; m/ChatProvider, so agent-o-rama auto-instruments it for tracing.
  (openai/declare-model
   topology
   "researcher"
   {:api-key   (System/getenv "OPENAI_API_KEY")
    :model     "gpt-5.1"
    :reasoning {:effort :high :summary :auto}
    ;; store? false + encrypted reasoning content = the model's chain of
    ;; thought survives the trip through Rama between loop iterations
    :store?    false})

  (t/new-tools-agent topology "tools" TOOLS)

  (->
    topology
    (aor/new-agent "ResearchAgent")

    ;; Self-looping node: each model turn is a separate traced node invoke,
    ;; exactly like the current react.clj — but with plain data throughout.
    (aor/node
     "research"
     "research"
     (fn [agent-node messages]
       (let [model      (aor/get-agent-object agent-node "researcher")
             tools      (aor/agent-client agent-node "tools")
             {:keys [message tool-calls text usage]}
             (m/chat model
                     {:messages messages
                      ;; web-search executes inside OpenAI (no round-trip);
                      ;; save_note comes back to us as a tool call
                      :tools    (into [(openai/web-search)] TOOLS)})]
         (if (seq tool-calls)
           ;; :message already contains the reasoning blocks + tool calls;
           ;; conj it, append tool results, loop. Nothing is lost.
           (let [tool-results (aor/agent-invoke tools tool-calls)]
             (aor/emit! agent-node "research"
                        (into (conj messages message) tool-results)))
           (aor/result! agent-node
                        {:report          text
                         :reasoning-tokens (:reasoning-tokens usage)})))))))

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              _   (aor/start-ui ipc)]
    (rtest/launch-module! ipc OpenAIResearchModule {:tasks 4 :threads 2})
    (let [manager (aor/agent-manager ipc (rama/get-module-name OpenAIResearchModule))
          agent   (aor/agent-client manager "ResearchAgent")]
      (aor/agent-invoke
       agent
       [(m/system "You are a meticulous research assistant. Save notes as you go.")
        (m/user "What changed in the EU AI Act implementation timeline this year?")]))))

;; What the trace UI shows for each "research" node invoke (neutral maps,
;; produced by the provider adapter instead of langchain4j_trace.clj):
;;
;;   model-call  {"provider" "openai" "model" "gpt-5.1"
;;                "reasoning" {"effort" "high"}
;;                "input"  [{"type" "system" ...} {"type" "user" ...}]
;;                "output" {"type" "ai"
;;                          "reasoningSummary" "Comparing the Council's..."
;;                          "toolRequests" [{"id" "call_1" "toolName" "save_note" ...}]}
;;                "usage" {"input" 812 "output" 331 "reasoning" 210}
;;                "builtInTools" [{"type" "web_search" "queries" [...]}]}
;;   tool-call   {"id" "call_1" "name" "save_note" "args" {...} "type" "success"}
