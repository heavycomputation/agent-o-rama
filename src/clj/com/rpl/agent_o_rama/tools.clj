;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent-o-rama.tools
  "Tools integration for AI agents.\n
\n
This namespace provides utilities for defining tools and tool agents for use with AI models via function calling. Tools allow AI agents to interact with external systems, perform calculations, and execute custom logic during conversation.\n
\n
Key concepts:\n
  - A tool is defined with [[tool]]: a plain-data spec (name, description, JSON schema for parameters) plus an implementation function
  - [[deftool]] is sugar for the common case, declaring the spec and the implementation in one form
  - Tool agents ([[new-tools-agent]]) execute the tool calls requested by AI models and return results
  - Error handlers control how tool execution failures are handled
\n
Example:\n
<pre>
(deftool add
  \"Add two numbers together\"
  [a (schema/number \"first number\")
   b (schema/number \"second number\")]
  (+ a b))
(new-tools-agent topology \"calculator\" [add])
</pre>
\n
[[deftool]] expands to [[tool]], which is what to reach for when tools are
built at runtime rather than at the top level:\n
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
</pre>"
  (:use [com.rpl.rama]
        [com.rpl.rama.path])
  (:require
   [com.rpl.agent-o-rama.impl.clojure :as c]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.impl.tools-impl :as tools-impl]
   [com.rpl.agent-o-rama.impl.types :as aor-types]
   [com.rpl.agent-o-rama.schema :as schema]
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
See also [[deftool]], which declares the spec and the implementation in one
form. Use this function directly for tools built at runtime — from config, a
database, or another service's tool list.\n
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

;; -- deftool ----------------------------------------------------------------
;;
;; deftool is pure syntax over [[tool]]: it expands to
;; (def <name> (tool <spec-map> <fn> <options>)) and introduces no runtime
;; concepts of its own. [[tool]] remains the primitive, and is still the way
;; to build tools dynamically (from config, a database, an MCP server's tool
;; list, ...).

(def ^:private DEFTOOL-OPTIONS
  #{:name :description :schema :strict? :additional-properties :context :as})

(defn- deftool-throw!
  [tool-name msg data]
  (throw (h/ex-info (str "deftool " tool-name ": " msg)
                    (assoc data :tool-name tool-name))))

(defn- deftool-param-key
  "The JSON argument name for a parameter symbol: its ^{:key \"...\"} metadata
  if present, otherwise the symbol's name."
  [tool-name sym]
  (let [k (:key (meta sym))]
    (cond
      (nil? k)    (clojure.core/name sym)
      (string? k) k
      :else       (deftool-throw! tool-name
                                  "parameter :key metadata must be a string"
                                  {:param sym :key k}))))

(defn- deftool-parse-params
  [tool-name params]
  (when-not (vector? params)
    (deftool-throw! tool-name
                    "expects a parameter vector of name/schema pairs"
                    {:params params}))
  (when (odd? (count params))
    (deftool-throw! tool-name
                    "parameter vector must contain name/schema pairs"
                    {:params params}))
  (let [parsed (mapv
                (fn [[sym schema]]
                  (when-not (simple-symbol? sym)
                    (deftool-throw! tool-name
                                    "parameter names must be simple symbols"
                                    {:param sym}))
                  ;; only the metadata deftool consumes is stripped; the
                  ;; rest (type hints, say) rides along to the binding
                  {:sym       (vary-meta sym dissoc :optional :key)
                   :key       (deftool-param-key tool-name sym)
                   :schema    schema
                   :optional? (clojure.core/boolean (:optional (meta sym)))})
                (partition 2 params))
        dupes  (->> parsed
                    (mapv :key)
                    frequencies
                    (filterv (fn [[_ n]] (> n 1)))
                    (mapv first))]
    (when (seq dupes)
      (deftool-throw! tool-name
                      "duplicate parameter names"
                      {:duplicates dupes}))
    parsed))

(defn- deftool-check-bindings!
  [tool-name param-syms context as-sym]
  (let [all   (concat param-syms context (when as-sym [as-sym]))
        dupes (->> all
                   frequencies
                   (filterv (fn [[_ n]] (> n 1)))
                   (mapv first))]
    (when (seq dupes)
      (deftool-throw! tool-name
                      "conflicting binding names"
                      {:conflicting (vec dupes)}))))

(defmacro deftool
  "Defines a tool as a var, in one form.\n
\n
This is sugar over [[tool]]: it expands to
`(def <name> (tool <spec-map> <fn> <options>))`, so what you get is an
ordinary ToolInfo, identical to one built by hand. Parameters are declared
once — as name/schema pairs — and are bound in the body, destructured out of
the JSON argument map by name.\n
\n
Args:\n
  - name - Symbol to def. Also the tool's name as seen by the model, unless
    the :name option overrides it
  - docstring - Optional string; becomes the tool's :description (and the
    var's docstring)
  - options - Optional map:
    - :name - String tool name, overriding the var name
    - :description - String description, overriding the docstring
    - :schema - Full parameter schema, replacing the one built from the
      parameter vector (parameters are still destructured from the arguments)
    - :strict? - Boolean, provider strict-mode flag where supported. Strict
      modes also require closed objects, so pass
      :additional-properties false alongside it
    - :additional-properties - Boolean, passed to
      com.rpl.agent-o-rama.schema/object
    - :context - Vector of two symbols, e.g. [agent-node caller-data], bound
      to the agent node and caller data. Supplying it sets :include-context?
    - :as - Symbol bound to the whole (string-keyed) argument map
  - params - Vector of name/schema pairs. Every parameter is required unless
    tagged ^:optional. Tag a parameter with ^{:key \"...\"} when the JSON
    argument name isn't a valid Clojure symbol. Any other metadata, such as a
    type hint, passes through to the binding
  - body - Tool implementation; its value is returned to the model\n
\n
Example:\n
<pre>
(deftool compound-interest
  \"Computes the final balance for principal p at annual rate r (percent)
   compounded yearly for n years\"
  [p (schema/number \"Principal amount\")
   r (schema/number \"Annual interest rate in percent\")
   n (schema/integer \"Number of years\")]
  (format \"%.2f\" (* p (Math/pow (+ 1.0 (/ r 100.0)) n))))
</pre>
\n
With agent context, an optional parameter, and a name the model sees
differently from the var:\n
<pre>
(deftool search-flights
  \"Search for available flights between airports\"
  {:name    \"search_flights\"
   :context [agent-node caller-data]}
  [departure-airport        (schema/string \"3-letter departure airport code\")
   arrival-airport          (schema/string \"3-letter arrival airport code\")
   ^:optional start-date    (schema/string \"Earliest departure date (YYYY-MM-DD)\")]
  (search (aor/get-store agent-node \"$$flights\")
          departure-airport
          arrival-airport
          start-date))
</pre>
\n
Tools built at runtime, rather than at the top level, use [[tool]] directly."
  {:arglists '([name docstring? options? params & body])}
  [tool-name & args]
  (when-not (simple-symbol? tool-name)
    (throw (h/ex-info "deftool expects a simple symbol name"
                      {:name tool-name})))
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [options args]   (if (map? (first args))
                           [(first args) (next args)]
                           [nil args])
        params           (first args)
        body             (next args)
        invalid          (remove DEFTOOL-OPTIONS (clojure.core/keys options))]
    (when (seq invalid)
      (deftool-throw! tool-name
                      "invalid options"
                      {:invalid (vec invalid)
                       :allowed (vec (sort DEFTOOL-OPTIONS))}))
    (when (empty? body)
      (deftool-throw! tool-name "requires a body" {}))
    (let [parsed  (deftool-parse-params tool-name params)
          context (:context options)
          as-sym  (:as options)]
      (when (and (some? context)
                 (not (and (vector? context)
                           (= 2 (count context))
                           (every? simple-symbol? context))))
        (deftool-throw!
         tool-name
         "the :context option must be a vector of two symbols, e.g. [agent-node caller-data]"
         {:context context}))
      (when (and (some? as-sym) (not (simple-symbol? as-sym)))
        (deftool-throw! tool-name
                        "the :as option must be a simple symbol"
                        {:as as-sym}))
      (deftool-check-bindings! tool-name (mapv :sym parsed) context as-sym)
      (let [args-sym  (or as-sym (gensym "args"))
            required  (->> parsed
                           (remove :optional?)
                           (mapv :key))
            schema    (if (contains? options :schema)
                        (:schema options)
                        `(schema/object
                          ~(cond-> {}
                             (seq required)
                             (assoc :required required)
                             (contains? options :additional-properties)
                             (assoc :additional-properties
                                    (:additional-properties options)))
                          ;; array-map so properties keep declaration order,
                          ;; whatever the parameter count
                          (array-map ~@(mapcat (juxt :key :schema) parsed))))
            spec      (cond-> {:name   (if (contains? options :name)
                                          (:name options)
                                          (clojure.core/name tool-name))
                               :schema schema}
                        (or (:description options) docstring)
                        (assoc :description (or (:description options)
                                                docstring))
                        (contains? options :strict?)
                        (assoc :strict? (:strict? options)))
            bindings  (into []
                            (mapcat (fn [{:keys [sym key]}]
                                      [sym `(get ~args-sym ~key)]))
                            parsed)
            fn-name   (symbol (str tool-name "-tool-fn"))
            fn-form   (if context
                        `(fn ~fn-name [~(first context) ~(second context) ~args-sym]
                           (let ~bindings ~@body))
                        `(fn ~fn-name [~args-sym]
                           (let ~bindings ~@body)))]
        `(def ~tool-name
           ~@(when docstring [docstring])
           (tool ~spec ~fn-form ~(when context {:include-context? true})))
      ))))

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
