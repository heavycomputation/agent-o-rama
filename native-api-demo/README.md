# Native provider API for agent-o-rama — feasibility & illustrative demos

**Status: exploration only. Nothing in this folder compiles or runs.** These files sketch
what a fork of agent-o-rama could look like with langchain4j removed and first-class,
latest-spec integrations for OpenAI (Responses API), Anthropic (Messages API), and xAI.

---

## Feasibility verdict: very feasible

A full audit of `src/` found lc4j coupling in only 13 files, concentrated in three
load-bearing seams. The orchestration core — agent graphs, nodes, `emit!`/`result!`,
stores, streaming depots, datasets, experiments, evaluators plumbing, and the entire
cljs UI — is already provider-agnostic.

| Seam | Where | What a fork does |
|---|---|---|
| Auto-instrumentation | `impl/agent_node.clj:610` `wrap-agent-object` — `instance? ChatModel` checks proxy lc4j models for tracing/streaming | Replace `instance?` dispatch with an AOR `ChatProvider` protocol. Any object satisfying it gets traced. ~180 lines, one file. |
| Trace conversion | `impl/langchain4j_trace.clj` — lc4j messages → neutral string-keyed maps | Delete. Native messages are *already* plain maps; the neutral trace format is unchanged, so the UI needs zero changes (it never sees lc4j — it consumes these maps). |
| Tools wire format | `impl/tools_impl.clj`, `ToolInfo.java` — `ToolExecutionRequest` / `ToolExecutionResultMessage` | Replace with plain maps `{:id :name :args}` / `{:role :tool ...}`. These lc4j types carry 3 fields each. |

The ~1200 lines of lc4j nippy/JSON serializers (`impl/serialize.clj`,
`impl/json_serialize.clj`) exist only because lc4j *Java objects* flow through Rama
depots between nodes. With messages as plain Clojure data, serialization is free and
those files are mostly deleted. The `aor/llm-judge` built-in evaluator
(`impl/evaluators.clj:115`) takes a model object and would accept a native one unchanged
in spirit.

One real cost to plan for: **persisted-data compatibility.** Existing PStates/datasets
contain nippy-frozen lc4j objects. A fork either keeps the thaw side of the old
serializers or accepts a clean break (fine for a personal fork).

## Why lc4j-free is *better* here, not just cosmetic

lc4j's `ChatModel` abstraction targets Chat Completions-era semantics. It cannot
express:

- **Reasoning + tool calling together** (OpenAI Responses API requires reasoning items
  to round-trip with tool results; lc4j drops them, which is why reasoning models
  degrade to non-reasoning behavior in tool loops).
- **Server-side built-in tools** (OpenAI `web_search`, `code_interpreter`; Anthropic
  server tools) — executed by the provider, no client round-trip.
- **Response chaining** (`previous_response_id`) and encrypted reasoning persistence.
- **Anthropic extended thinking** blocks with signatures preserved across tool turns.
- Streaming of reasoning summaries / partial tool calls (the current AOR wrapper only
  intercepts `onPartialResponse(String)`).

A native integration makes these first-class *and* they trace beautifully because the
neutral message format carries reasoning/tool blocks as data the UI can render.

## Proposed design (what these demos assume)

**Messages are plain Clojure maps** — nippy-serializable for free, greppable in traces:

```clojure
{:role :system :content "You are..."}
{:role :user   :content "..."}                          ; string sugar
{:role :assistant
 :content [{:type :reasoning :summary "..." :provider-data {...}}  ; opaque, round-trips
           {:type :text :text "..."}
           {:type :tool-call :id "call_1" :name "tavily" :args {"terms" "..."}}]}
{:role :tool :tool-call-id "call_1" :name "tavily" :content "..."}
```

**One neutral response shape** from `m/chat` regardless of provider:

```clojure
{:message ...            ; assistant message map, splice straight into history
 :text "..."             ; concatenated text convenience
 :tool-calls [{:id "call_1" :name "tavily" :args {...}}]
 :parsed {...}           ; present when :output-schema was given
 :finish-reason :tool-calls        ; :stop | :length | :refusal | :tool-calls
 :usage {:input-tokens 812 :output-tokens 331 :reasoning-tokens 210}
 :provider {:name :openai :response-id "resp_abc"}}
```

**New namespaces** (see `proposed_api.clj` for the full sketch):

- `com.rpl.agent-o-rama.model` — `ChatProvider` protocol, `chat`, message helpers,
  `new-tool-loop-agent` convenience graph builder.
- `com.rpl.agent-o-rama.model.openai` — `responses-model`, `declare-model`, built-in
  tool constructors (`web-search`, `code-interpreter`).
- `com.rpl.agent-o-rama.model.anthropic` — `messages-model`, `declare-model`,
  extended-thinking config, server tools.
- `com.rpl.agent-o-rama.model.xai` — `model`, `declare-model`, Live Search params.
- `com.rpl.agent-o-rama.schema` — JSON-schema helpers producing plain maps
  (replaces `com.rpl.agent-o-rama.langchain4j.json`).
- `com.rpl.agent-o-rama.tools` — same graph machinery, but `tool` takes a data map +
  fn; tool results are plain maps. `deftool` is sugar over `tool`: it declares the
  spec and the implementation in one form and binds the model's arguments as
  parameters, expanding to `(def <name> (tool <spec> <fn> <options>))`.

**Tracing/streaming stay automatic.** `declare-agent-object-builder` still auto-wraps:
the fork's `wrap-agent-object` dispatches on `(satisfies? ChatProvider obj)` instead of
`(instance? ChatModel obj)`. A model built with `:stream? true` auto-emits text deltas
via the existing `stream-chunk!` depot machinery — `aor/agent-stream` on the client is
untouched.

## Files in this folder

| File | Demonstrates |
|---|---|
| `proposed_api.clj` | Signature-level sketch of every new namespace |
| `openai_research_agent.clj` | Responses API: reasoning effort + custom tools + built-in `web_search`, reasoning round-trip through a tool loop |
| `anthropic_thinking_agent.clj` | Messages API: extended thinking + tools + streaming, thinking blocks preserved across turns |
| `xai_structured_agent.clj` | Grok: Live Search, structured output (`:output-schema` → `:parsed`), streaming |
| `provider_agnostic_agent.clj` | The same node code running against all three providers; the `new-tool-loop-agent` one-liner |

## Rough fork task list

1. Define the neutral message/response/tool formats (pure data; this folder is the draft).
2. `com.rpl.agent-o-rama.model` protocol + `chat`; rewrite `wrap-agent-object` dispatch
   and `record-model-call!` against the protocol (`impl/agent_node.clj:503-784`).
3. HTTP layer: JDK `java.net.http.HttpClient` (SSE-capable, zero new deps) or http-kit
   (already a dep). One thin client per provider (~200-400 lines each).
4. Rewrite `impl/tools_impl.clj` + `tools.clj` against map-based tool calls; port
   `langchain4j/json.clj` to `schema.clj` emitting plain maps.
5. Delete `langchain4j.clj`, `langchain4j/json.clj`, `impl/langchain4j_trace.clj`, and
   ~90% of the two serializer files; drop the lc4j dep from `project.clj`.
6. Port `aor/llm-judge` evaluator to take a native model.
7. (Optional) Update 4 substring matches in `ui/components/conversation.cljs` that
   key off `"UserMessage"`-style class names — or emit role-keyed maps and simplify.

The public orchestration API (`defagentmodule`, `node`, `emit!`, `result!`, stores,
datasets, evaluators, experiments, `agent-stream`) is untouched.
