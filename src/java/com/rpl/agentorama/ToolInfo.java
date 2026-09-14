// Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
package com.rpl.agentorama;

import java.util.Map;

import com.rpl.agentorama.impl.AORHelpers;
import com.rpl.rama.ops.*;

/**
 * Combines a tool specification with its implementation function.
 *
 * ToolInfo is used to define tools that can be called by AI models in tools agents
 * created with {@link AgentTopology#newToolsAgent(String, java.util.List)}. The
 * specification is a plain map describing the tool's interface: "name" (required),
 * "description", and "schema" (a JSON schema map for the parameters).
 *
 * Example:
 * <pre>{@code
 * Map spec = Map.of(
 *   "name", "calculator",
 *   "description", "Performs basic arithmetic operations",
 *   "schema", jsonSchemaMap);
 *
 * ToolInfo toolInfo = ToolInfo.create(spec, (Map args) -> {
 *   int a = (Integer) args.get("a");
 *   int b = (Integer) args.get("b");
 *   return String.valueOf(a + b);
 * });
 *
 * // Use with newToolsAgent
 * topology.newToolsAgent("my-tools-agent", Arrays.asList(toolInfo));
 * }</pre>
 */
public interface ToolInfo {
  /**
   * Creates a tool info with a simple function implementation.
   *
   * @param spec the tool specification map defining the tool's interface
   * @param toolFn the function that implements the tool's behavior
   * @param <T1> the type of the tool function's input
   * @return the tool info instance
   */
  static <T1> ToolInfo create(Map<String, Object> spec, RamaFunction1<Map<String, T1>, String> toolFn) {
    return (ToolInfo) AORHelpers.CREATE_TOOL_INFO.invoke(spec, toolFn);
  }

  /**
   * Creates a tool info with a function that has access to the agent node context.
   *
   * This allows the tool function to access agent objects, stores, and other
   * agent execution context.
   *
   * @param spec the tool specification map defining the tool's interface
   * @param toolFn the function that implements the tool's behavior with agent context
   * @param <T1> the type of the tool function's first input
   * @param <T2> the type of the tool function's second input
   * @return the tool info instance
   */
  static <T1, T2> ToolInfo createWithContext(Map<String, Object> spec, RamaFunction3<AgentNode, T1, Map<String, T2>, String> toolFn) {
    return (ToolInfo) AORHelpers.CREATE_TOOL_INFO_WITH_CONTEXT.invoke(spec, toolFn);
  }

  /**
   * Gets the tool specification map for this tool.
   *
   * @return the tool specification map
   */
  Map<Object, Object> getToolSpecification();
}
