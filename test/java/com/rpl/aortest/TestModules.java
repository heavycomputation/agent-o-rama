package com.rpl.aortest;

import com.rpl.agentorama.*;
import com.rpl.rama.RamaModule;
import com.rpl.rama.test.InProcessCluster;
import com.rpl.rama.test.LaunchConfig;

import clojure.lang.Keyword;
import clojure.lang.PersistentHashMap;

import java.util.*;


public class TestModules {
  static Keyword kw(String name) {
    return Keyword.intern(name);
  }

  static Map toolCall(String id, String name, String argsJson) {
    // a Clojure map, so it serializes through depots; args as a JSON string
    // (the tools agent parses string args, as providers deliver them)
    return PersistentHashMap.create(kw("id"), id, kw("name"), name, kw("args"), argsJson);
  }

  static Map<String, Object> intParamsSchema() {
    return Map.of(
      "type", "object",
      "properties", Map.of(
        "a", Map.of("type", "integer"),
        "b", Map.of("type", "integer")),
      "required", List.of("a", "b"));
  }

  public static class BasicToolsAgent extends AgentModule {
    public static List<ToolInfo> TOOLS = Arrays.asList(
      ToolInfo.create(
        Map.of("name", "add",
               "description", "Add two integers",
               "schema", intParamsSchema()),
        (Map<String, Integer> args) -> {
          return "" + (args.get("a") + args.get("b"));
        }),
      ToolInfo.createWithContext(
        Map.of("name", "multiply",
               "description", "Multiply two integers",
               "schema", intParamsSchema()),
        (AgentNode node, Integer callerData, Map<String, Integer> args) -> {
          return "" + (args.get("a") * args.get("b") + callerData);
        })
      );

    public static void doToolCall(AgentNode node, String k, Map request) {
      AgentClient tools = node.getAgentClient("tools");
      List<Map> results = tools.invoke(Arrays.asList(request), 6);
      if(results.size() != 1) throw new RuntimeException("failed");
      node.emit("agg", k, results.get(0).get(kw("content")));
    }

    @Override
    protected void defineAgents(AgentTopology topology) {
      topology.newToolsAgent("tools", TOOLS);
      topology.newToolsAgent("tools2", TOOLS, ToolsAgentOptions.errorHandlerStaticString("edcba"));
      topology.newAgent("foo")
              .node(
                "start",
                "a",
                (AgentNode node) -> {
                  node.emit("a");
                })
              .aggStartNode(
                "a",
                "tool",
                (AgentNode node) -> {
                  node.emit("tool", "a", toolCall("c1", "add", "{\"a\": 5, \"b\": 3}"));
                  node.emit("tool", "m", toolCall("c2", "multiply", "{\"a\": 6, \"b\": 8}"));
                  return null;
                })
              .node(
                "tool",
                List.of("agg"),
                BasicToolsAgent::doToolCall)
              .aggNode(
                "agg",
                null,
                BuiltIn.MAP_AGG,
                (AgentNode node, Map ret, Object aggStartRes) -> {
                  node.result(ret);
                });
    }
  }

  public static Map runBasicToolsAgent() throws Exception {
    try(InProcessCluster ipc = InProcessCluster.create()) {
      RamaModule module = new BasicToolsAgent();
      ipc.launchModule(module, new LaunchConfig(4, 2));
      AgentManager manager = AgentManager.create(ipc, module.getModuleName());
      AgentClient foo = manager.getAgentClient("foo");
      return foo.invoke();
    }
  }
}
