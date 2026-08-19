(ns com.rpl.agent-o-rama.tools
  "Tools integration for AI agents.\n
\n
This namespace provides utilities for defining tools and tool agents for use with AI models via function calling. Tools allow AI agents to interact with external systems, perform calculations, and execute custom logic during conversation.\n
\n
Key concepts:\n
  - A tool is defined with [[tool]]: a plain-data spec (name, description, JSON schema for parameters) plus an implementation function
  - Tool agents ([[new-tools-agent]]) execute the tool calls requested by AI models and return results
  - Error handlers control how tool execution failures are handled
\n
Example:\n
<pre>
(def calculator-tool
  (tool
    {:name        \"add\"
     :description \"Add two numbers together\"
     :schema      (schema/object
                   {:required [\"a\" \"b\"]}
                   {\"a\" (schema/number \"first number\")
                    \"b\" (schema/number \"second number\")})}
    (fn [args] (+ (get args \"a\") (get args \"b\")))))
(new-tools-agent topology \"calculator\" [calculator-tool])
</pre>"
  (:use [com.rpl.rama]
        [com.rpl.rama.path])
  (:require
   [com.rpl.agent-o-rama.impl.clojure :as c]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.tools-impl :as tools-impl]
   [com.rpl.agent-o-rama.impl.types :as aor-types]
   [com.rpl.rama.aggs :as aggs]))

(defn tool
  "Creates a complete tool definition from a plain-data spec and an
  implementation function.\n
\n
Tools created this way work with [[new-tools-agent]] and with native model
integrations (the :tools request key of com.rpl.agent-o-rama.model/chat).
When invoked through a tools agent, requests are neutral tool-call maps
{:id ... :name ... :args ...} (a response's :tool-calls entries) and results
are {:role :tool ...} messages ready to append to the conversation.\n
\n
Args:\n
  - spec - Map describing the tool interface:
    - :name - String tool name (required, unique within a tools agent)
    - :description - String description of what the tool does
    - :schema - JSON schema map for the parameters (see com.rpl.agent-o-rama.schema)
    - :strict? - Boolean, provider strict-mode flag where supported
  - tool-fn - Function implementing the tool. Takes either:
    - (args) - Just the parsed arguments map
    - (agent-node caller-data args) - Agent node, caller data, and arguments
  - options - Optional map with configuration:
    - :include-context? - Boolean, whether to pass agent-node and caller-data to tool-fn (default false)
\n
Returns:\n
  - ToolInfo - Complete tool definition for use with [[new-tools-agent]]\n
\n
Example:\n
<pre>
(tool
  {:name        \"add\"
   :description \"Add two numbers together\"
   :schema      (schema/object
                 {:required [\"a\" \"b\"]}
                 {\"a\" (schema/number \"first number\")
                  \"b\" (schema/number \"second number\")})}
  (fn [args] (+ (get args \"a\") (get args \"b\"))))
</pre>"
  ([spec tool-fn]
   (tool spec tool-fn nil))
  ([spec tool-fn options]
   (let [options (merge {:include-context? false} options)]
     (h/validate-options! spec
                          options
                          {:include-context? h/boolean-spec})
     (when-not (and (map? spec) (string? (:name spec)))
       (throw (h/ex-info "Invalid tool spec — must be a map with a :name string"
                         {:spec spec})))
     (when-not (ifn? tool-fn)
       (throw (h/ex-info "Invalid tool function" {:type (class tool-fn)})))
     (aor-types/->ToolInfoImpl spec
                               tool-fn
                               (:include-context? options)))))

(defn error-handler-static-string
  "Creates an error handler that always returns a static string for any exception.\n
\n
This is useful for providing user-friendly error messages back to a model when tool execution fails, rather than exposing technical exception details.\n
\n
Args:\n
  - s - String to return for any tool execution error
\n
Returns:\n
  - Function - Error handler that takes an exception and returns the string
\n
Example:\n
<pre>
(new-tools-agent topology \"calculator\" tools
  {:error-handler (error-handler-static-string \"Something went wrong. Please try again.\")})
</pre>"
  [s]
  (constantly s))

(defn error-handler-rethrow
  "Creates an error handler that re-throws exceptions without modification.\n
\n
This is useful when you want tool execution errors to propagate up to the calling agent, allowing it to handle the error in its own logic.\n
\n
Returns:\n
  - Function - Error handler that re-throws any exception
\n
Example:\n
<pre>
(new-tools-agent topology \"calculator\" tools
  {:error-handler (error-handler-rethrow)})
</pre>"
  []
  (fn [e] (throw e)))

(defn error-handler-default
  "Creates the default error handler that formats exceptions as user-friendly messages.\n
\n
This handler converts exceptions to readable error messages with a standard format: \"Error: <exception details>\\nPlease fix your mistakes.\"\n
\n
Returns:\n
  - Function - Error handler that formats exceptions as strings
\n
Example:\n
<pre>
(new-tools-agent topology \"calculator\" tools
  {:error-handler (error-handler-default)})
</pre>"
  []
  (fn [e]
    (tools-impl/tool-error-string (h/throwable->str e))))

(defn error-handler-by-type
  "Creates an error handler that handles different exception types differently.\n
\n
This handler matches exceptions by type and applies the corresponding handler function. If no type matches, the exception is re-thrown.\n
\n
Args:\n
  - tuples - Vector of [exception-type handler-function] pairs
\n
Returns:\n
  - Function - Error handler that dispatches by exception type
\n
Example:\n
<pre>
(new-tools-agent topology \"calculator\" tools
  {:error-handler (error-handler-by-type
                    [[ArithmeticException (fn [e] \"Math error occurred\")]
                     [IllegalArgumentException (fn [e] \"Invalid input provided\")]])})
</pre>"
  [tuples]
  (fn [e]
    (if-let [ret (reduce
                  (fn [_ [ex-type afn]]
                    (when (instance? ex-type e)
                      (reduced (afn e))))
                  nil
                  tuples)]
      ret
      (throw e)
    )))

(defn error-handler-static-string-by-type
  "Creates an error handler that returns static strings for different exception types.\n
\n
This is a convenience function that combines [[error-handler-by-type]] with [[error-handler-static-string]] to provide simple string responses for different exception types.\n
\n
Args:\n
  - tuples - Vector of [exception-type string] pairs
\n
Returns:\n
  - Function - Error handler that returns strings based on exception type
\n
Example:\n
<pre>
(new-tools-agent topology \"calculator\" tools
  {:error-handler (error-handler-static-string-by-type
                    [[ArithmeticException \"Math error occurred\"]
                     [IllegalArgumentException \"Invalid input provided\"]
                     [ClassCastException \"Type conversion failed\"]])})
</pre>"
  [tuples]
  (let [tuples (transform [(view vec) ALL LAST] (fn [s] (constantly s)) tuples)]
    (error-handler-by-type tuples)))

(defn new-tools-agent
  "Creates a tools agent that can execute tool calls from AI models.\n
\n
A tools agent is a special type of agent designed to execute tool calls requested by AI models. It processes batches of tool execution requests, executes the corresponding tool functions, and returns results back to the calling agent.\n
\n
The agent uses aggregation to collect results from parallel tool executions and returns them as a vector of {:role :tool ...} messages ready to append to the conversation.\n
\n
Args:\n
  - topology - agent topology instance
  - name - String name for the tools agent
  - tools - Collection of ToolInfo instances created with [[tool]]
  - options - Optional map with configuration:
    - :error-handler - Function that handles tool execution errors (default: [[error-handler-default]])
\n
Example:\n
<pre>
(let [calculator-tool
      (tool
        {:name        \"add\"
         :description \"Add two numbers together\"
         :schema      (schema/object
                       {:required [\"a\" \"b\"]}
                       {\"a\" (schema/number \"first number\")
                        \"b\" (schema/number \"second number\")})}
        (fn [args] (+ (get args \"a\") (get args \"b\"))))]
  (new-tools-agent topology \"calculator\" [calculator-tool]))
;; With custom error handling
(new-tools-agent topology \"robust-calculator\" tools
  {:error-handler (error-handler-static-string \"Calculation failed\")})
</pre>"
  ([topology name tools]
   (new-tools-agent topology name tools nil))
  ([topology name tools options]
   (tools-impl/hook:new-tools-agent-options name options)
   (let [options (merge {:error-handler (error-handler-default)}
                        options)]
     (h/validate-options! name
                          options
                          {:error-handler h/fn-spec})
     (-> topology
         (c/new-agent name)
         (c/agg-start-node
          "begin"
          "tool"
          (fn begin
            ([agent-node requests]
             (begin agent-node requests nil))
            ([agent-node requests caller-data]
             (doseq [r requests]
               (c/emit! agent-node "tool" r caller-data)))))
         (c/node
          "tool"
          "agg-results"
          (tools-impl/mk-tool-fn tools (:error-handler options)))
         (c/agg-node
          "agg-results"
          nil
          aggs/+vec-agg
          (fn [agent-node agg-state _]
            (c/result! agent-node agg-state)))
     ))))
