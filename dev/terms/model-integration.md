# Model Integration

## Definition
Native, provider-neutral integration with LLM providers (OpenAI Responses
API, Anthropic Messages API, xAI) built on the ChatProvider protocol.

## Architecture Role
AI abstraction layer providing model interfaces, structured output, and
tool calling. Messages, requests, responses, and streaming deltas are all
plain Clojure data.

Any declared agent object satisfying `ChatProvider` is auto-wrapped for
trace recording (model params, messages, token usage incl. reasoning
tokens, time-to-first-token) and automatic streaming (text deltas emitted
via `aor/stream-chunk!`). The underlying provider object can be accessed
using `IUnderlying.getUnderlying` on the wrapped object.

Provider adapters round-trip raw provider payloads (OpenAI reasoning
items, Anthropic thinking blocks) through `:provider-data` on assistant
messages, so reasoning survives tool loops.

## Operations
- Chat model interactions: `model/chat`
- Structured output: `:output-schema` request key -> `:parsed` response key
- Tool calling: `:tools` request key -> `:tool-calls` response key
- Streaming: `:stream?` model/request option, optional on-delta handler

## Invariants
- Provider-agnostic request/response formats
- agent-object-builder on a ChatProvider returns an instrumented ChatProvider

## Key Clojure API
- Primary functions: `chat`, message constructors (`system`, `user`,
  `assistant`, `tool-result`)
- Providers: `com.rpl.agent-o-rama.model.openai/responses-model`,
  `com.rpl.agent-o-rama.model.anthropic/messages-model`,
  `com.rpl.agent-o-rama.model.xai/model` (each with `declare-model` sugar)
- Access: `com.rpl.agent-o-rama.model` namespace; schemas via
  `com.rpl.agent-o-rama.schema`

## Relationships
- Uses: [Agent Objects](agent-objects.md)
- Used by: [Tool Calling](tool-calling.md)

## Examples
- Clojure: `examples/clj/src/com/rpl/agent/basic/openai_agent.clj`
