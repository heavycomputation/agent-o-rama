(ns com.rpl.agent-o-rama.schema
  "Helpers for building JSON schemas as plain Clojure maps.

  These produce ordinary string-keyed maps in standard JSON Schema shape,
  usable directly as tool parameter schemas (com.rpl.agent-o-rama.tools/tool)
  and structured-output schemas (the :output-schema request key in
  com.rpl.agent-o-rama.model/chat). Being plain data, they serialize through
  Rama and can be built/inspected/transformed with normal Clojure functions.

  This is the provider-neutral replacement for
  com.rpl.agent-o-rama.langchain4j.json, and follows the same argument
  conventions.

  Example:
  <pre>
  (schema/object
   {:description \"Parameters for calculator operations\"
    :required    [\"operation\" \"a\" \"b\"]}
   {\"operation\" (schema/enum \"The arithmetic operation to perform\"
                              [\"add\" \"subtract\" \"multiply\" \"divide\"])
    \"a\"         (schema/number \"The first number\")
    \"b\"         (schema/number \"The second number\")})
  </pre>"
  (:refer-clojure :exclude [boolean]))

(defn- with-description
  [m description]
  (if description
    (assoc m "description" description)
    m))

(defn object
  "Creates an object schema from a map of property name (string) to property
  schema. The 1-arity takes just the properties; the 2-arity takes an options
  map first:
    :description           - String description of the object
    :required              - Collection of required property names
    :additional-properties - Boolean; set false for strict/structured-output
                             modes that demand closed objects"
  ([properties]
   (object nil properties))
  ([{:keys [description required additional-properties]} properties]
   (cond-> {"type"       "object"
            "properties" properties}
     description                      (assoc "description" description)
     (seq required)                   (assoc "required" (vec required))
     (some? additional-properties)    (assoc "additionalProperties"
                                             additional-properties))))

(defn strict-object
  "Like [[object]], but suitable for strict structured outputs: every
  property is required and additional properties are disallowed, as strict
  mode demands."
  ([properties]
   (strict-object nil properties))
  ([options properties]
   (object (merge options
                  {:required              (vec (keys properties))
                   :additional-properties false})
           properties)))

(defn string
  ([] (string nil))
  ([description] (with-description {"type" "string"} description)))

(defn number
  ([] (number nil))
  ([description] (with-description {"type" "number"} description)))

(defn integer
  ([] (integer nil))
  ([description] (with-description {"type" "integer"} description)))

(defn boolean
  ([] (boolean nil))
  ([description] (with-description {"type" "boolean"} description)))

(defn enum
  "Creates a string enum schema restricted to the given values."
  ([values] (enum nil values))
  ([description values]
   (with-description {"type" "string"
                      "enum" (vec values)}
                     description)))

(defn array
  "Creates an array schema whose items match the given schema."
  ([items-schema] (array nil items-schema))
  ([description items-schema]
   (with-description {"type"  "array"
                      "items" items-schema}
                     description)))
