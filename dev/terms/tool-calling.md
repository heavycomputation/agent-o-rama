# Tool Calling

## Definition
Integration pattern connecting AI models with external functions and APIs.

## Architecture Role
Enables AI-driven external system interaction. Provides structured interface for model-to-function communication.

## Operations
- Define tool specifications
- Execute tool functions
- Parse tool responses
- Handle errors gracefully

## Invariants
- Type-safe tool definitions
- Deterministic execution
- Error propagation

## Key Clojure API
- Primary functions: `tools/deftool`, `tools/tool`, `new-tools-agent`
- Creation: `(deftool name docstring? options? params & body)`, or
  `(tool spec tool-fn [options])` for tools built at runtime
- Access: `src/clj/com/rpl/agent_o_rama/tools.clj`

## Key Java API
- Primary functions: Tool interface implementations
- Creation: Via builder patterns
- Access: plain-data tool specs tool interfaces

## Relationships
- Uses: [plain-data tool specs Integration](model-integration.md), [Tools Sub Agent](tools-sub-agent.md)
- Used by: [Agent Node](agent-node.md)

## Examples
- Clojure: `examples/clj/src/com/rpl/agent/basic/tools_agent.clj`
- Java: `examples/java/react/src/main/java/com/rpl/agent/react/ReActExample.java`