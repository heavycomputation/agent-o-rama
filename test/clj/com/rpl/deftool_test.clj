(ns com.rpl.deftool-test
  "Tests for the deftool macro. deftool is sugar over tools/tool, so most of
  these assert that it produces exactly what the equivalent hand-written
  tools/tool call produces."
  (:use [clojure.test]
        [com.rpl.test-helpers]
        [com.rpl.rama])
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.impl.anthropic :as ianthropic]
   [com.rpl.agent-o-rama.impl.openai :as iopenai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama.test :as rtest]))

;;; -- basics ----------------------------------------------------------------

(tools/deftool add
  "Add two numbers together"
  [a (schema/number "first number")
   b (schema/number "second number")]
  (+ a b))

(def ADD-BY-HAND
  (tools/tool
   {:name        "add"
    :description "Add two numbers together"
    :schema      (schema/object
                  {:required ["a" "b"]}
                  {"a" (schema/number "first number")
                   "b" (schema/number "second number")})}
   (fn [args] (+ (get args "a") (get args "b")))))

(deftest equivalent-to-tool-test
  ;; the whole premise of the macro: identical data out
  (is (= (:tool-specification ADD-BY-HAND)
         (:tool-specification add)))
  (is (= (:include-context? ADD-BY-HAND)
         (:include-context? add)))
  (is (= (class ADD-BY-HAND) (class add)))
  (is (= 3 ((:tool-fn add) {"a" 1 "b" 2})))
  ;; missing arguments read as nil, exactly as (get args "b") would
  (is (thrown? Exception ((:tool-fn add) {"a" 1}))))

(deftest var-metadata-test
  ;; the docstring does double duty as the tool description and the var doc
  (is (= "Add two numbers together" (:doc (meta #'add))))
  (is (= "Add two numbers together"
         (:description (:tool-specification add)))))

(defn- expand
  "macroexpand-1 resolves against *ns*, which is not this namespace when the
  test runner invokes the var."
  [form]
  (binding [*ns* (find-ns 'com.rpl.deftool-test)]
    (macroexpand-1 form)))

(deftest macroexpansion-test
  ;; expansion is a plain def of a tools/tool call — no new runtime concepts
  (let [[def-sym name-sym docstring tool-call]
        (expand '(tools/deftool add "Add" [a (schema/number)] (+ a 1)))]
    (is (= 'def def-sym))
    (is (= 'add name-sym))
    (is (= "Add" docstring))
    (is (= 'com.rpl.agent-o-rama.tools/tool (first tool-call)))
    (is (= {:name   "add"
            :schema '(com.rpl.agent-o-rama.schema/object
                      {:required ["a"]}
                      (clojure.core/array-map "a" (schema/number)))
            :description "Add"}
           (second tool-call)))
    (is (nil? (nth tool-call 3)))))

;;; -- parameters ------------------------------------------------------------

(tools/deftool describe
  "Describes something"
  [text                  (schema/string "the text")
   ^:optional style      (schema/enum "how to describe it" ["terse" "florid"])
   ^{:key "max.length"} max-length (schema/integer "cap")]
  {:text text :style style :max-length max-length})

(deftest optional-params-test
  ;; only non-optional params land in :required
  (is (= ["text" "max.length"]
         (get-in (:tool-specification describe) [:schema "required"]))))

(deftest key-metadata-test
  ;; ^{:key "..."} decouples the JSON argument name from the Clojure symbol
  (is (contains? (get-in (:tool-specification describe) [:schema "properties"])
                 "max.length"))
  (is (= {:text "hi" :style "terse" :max-length 10}
         ((:tool-fn describe) {"text"       "hi"
                               "style"      "terse"
                               "max.length" 10}))))

(tools/deftool many-params
  "More parameters than an array map holds"
  [p1 (schema/string) p2 (schema/string) p3 (schema/string)
   p4 (schema/string) p5 (schema/string) p6 (schema/string)
   p7 (schema/string) p8 (schema/string) p9 (schema/string)
   p10 (schema/string)]
  [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10])

(deftest property-order-test
  ;; properties keep declaration order past the array-map/hash-map boundary,
  ;; so the wire schema reads the way it was written
  (is (= ["p1" "p2" "p3" "p4" "p5" "p6" "p7" "p8" "p9" "p10"]
         (keys (get-in (:tool-specification many-params)
                       [:schema "properties"]))))
  (is (= ["p1" "p2" "p3" "p4" "p5" "p6" "p7" "p8" "p9" "p10"]
         (get-in (:tool-specification many-params) [:schema "required"]))))

(tools/deftool no-params
  "Takes nothing"
  []
  "ok")

(deftest no-params-test
  (is (= {"type" "object" "properties" {}}
         (:schema (:tool-specification no-params))))
  (is (= "ok" ((:tool-fn no-params) {}))))

;;; -- options ---------------------------------------------------------------

(tools/deftool greet
  "Greet using agent context"
  {:name    "greet_person"
   :context [agent-node caller-data]}
  [who (schema/string "who to greet")]
  (str "hello " who " from " caller-data " on " agent-node))

(deftest context-option-test
  (is (= "greet_person" (:name (:tool-specification greet))))
  (is (true? (:include-context? greet)))
  (is (= "hello world from data on node"
         ((:tool-fn greet) "node" "data" {"who" "world"}))))

(tools/deftool raw-args
  "Sees the whole argument map"
  {:as args}
  [who (schema/string "who")]
  (str who "/" (get args "extra")))

(deftest as-option-test
  (is (= "ann/x" ((:tool-fn raw-args) {"who" "ann" "extra" "x"}))))

(def TODO-SCHEMA
  (schema/object
   {:description "A todo item"
    :required    ["title"]}
   {"title" (schema/string "what to do")
    "done"  (schema/boolean "whether it is finished")}))

(tools/deftool create-todo
  "Creates a todo"
  {:schema TODO-SCHEMA}
  [title (schema/string "ignored — :schema wins")]
  (str "created " title))

(deftest schema-override-test
  ;; a reusable schema replaces the generated one, but parameters are still
  ;; destructured out of the arguments
  (is (= TODO-SCHEMA (:schema (:tool-specification create-todo))))
  (is (= "created write tests"
         ((:tool-fn create-todo) {"title" "write tests"}))))

(tools/deftool strict-lookup
  "Looks something up"
  {:description          "overrides the docstring"
   :strict?              true
   :additional-properties false}
  [q (schema/string "query")]
  q)

(deftest description-and-strict-options-test
  (let [spec (:tool-specification strict-lookup)]
    (is (= "overrides the docstring" (:description spec)))
    (is (true? (:strict? spec)))
    (is (false? (get-in spec [:schema "additionalProperties"])))))

(tools/deftool ^:private hidden
  "Not part of the public surface"
  []
  "shh")

(deftest name-metadata-test
  ;; metadata on the name symbol passes through to the var, as with def
  (is (true? (:private (meta #'hidden)))))

;;; -- wire conversion -------------------------------------------------------

(deftest tool->wire-test
  ;; a deftool tool is an ordinary ToolInfo, so every adapter takes it
  (is (= {"type"        "function"
          "name"        "add"
          "description" "Add two numbers together"
          "parameters"  (:schema (:tool-specification add))}
         (iopenai/tool->wire add)))
  (is (= {"name"         "add"
          "description"  "Add two numbers together"
          "input_schema" (:schema (:tool-specification add))}
         (ianthropic/tool->wire add))))

;;; -- compile-time errors ---------------------------------------------------

(defn- expand-error
  "Root-cause message from expanding form. The compiler wraps errors thrown by
  a macro, so the message deftool produced is on the cause."
  [form]
  (try
    (expand form)
    "no error thrown"
    (catch Throwable t
      (.getMessage ^Throwable (or (ex-cause t) t)))))

(deftest invalid-params-test
  (is (re-find #"parameter vector must contain name/schema pairs"
               (expand-error '(tools/deftool t "d" [a] a))))
  (is (re-find #"expects a parameter vector"
               (expand-error '(tools/deftool t "d" a "body"))))
  (is (re-find #"parameter names must be simple symbols"
               (expand-error '(tools/deftool t "d" [other/a (schema/string)] a))))
  (is (re-find #"duplicate parameter names"
               (expand-error '(tools/deftool t
                                "d"
                                [a (schema/string) ^{:key "a"} b (schema/string)]
                                a))))
  (is (re-find #"parameter :key metadata must be a string"
               (expand-error
                '(tools/deftool t "d" [^{:key :a} a (schema/string)] a)))))

(deftest invalid-options-test
  (is (re-find #"invalid options"
               (expand-error '(tools/deftool t "d" {:nope 1} [] "x"))))
  (is (re-find #":context option must be a vector of two symbols"
               (expand-error '(tools/deftool t "d" {:context [node]} [] "x"))))
  (is (re-find #":as option must be a simple symbol"
               (expand-error '(tools/deftool t "d" {:as "args"} [] "x"))))
  (is (re-find #"conflicting binding names"
               (expand-error '(tools/deftool t "d" {:as a} [a (schema/string)] a))))
  (is (re-find #"conflicting binding names"
               (expand-error '(tools/deftool t
                                "d"
                                {:context [a caller-data]}
                                [a (schema/string)]
                                a)))))

(deftest missing-body-test
  (is (re-find #"requires a body"
               (expand-error '(tools/deftool t "d" []))))
  (is (re-find #"deftool expects a simple symbol name"
               (expand-error '(tools/deftool "t" "d" [] "x")))))

;;; -- end to end ------------------------------------------------------------

(tools/deftool multiply
  "Multiply two numbers"
  [a (schema/number "first number")
   b (schema/number "second number")]
  (* a b))

(tools/deftool echo-caller
  "Echo the caller data"
  {:context [_agent-node caller-data]}
  [prefix (schema/string "prefix")]
  (str prefix caller-data))

(deftest tools-agent-integration-test
  ;; deftool-defined tools run through a real tools agent unchanged
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind module
       (aor/agentmodule
        [topology]
        (tools/new-tools-agent topology "tools" [multiply echo-caller])
        (-> topology
            (aor/new-agent "caller")
            (aor/node
             "start"
             nil
             (fn [agent-node requests]
               (let [tools-agent (aor/agent-client agent-node "tools")]
                 (aor/result! agent-node
                              (aor/agent-invoke tools-agent requests "!"))))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind caller (aor/agent-client (aor/agent-manager ipc module-name)
                                    "caller"))
     (bind res
       (sort-by :tool-call-id
                (aor/agent-invoke
                 caller
                 [{:id "c1" :name "multiply" :args {"a" 6 "b" 7}}
                  {:id "c2" :name "echo-caller" :args {"prefix" "hi"}}])))
     (is (= [{:role         :tool
              :tool-call-id "c1"
              :name         "multiply"
              :content      "42"}
             {:role         :tool
              :tool-call-id "c2"
              :name         "echo-caller"
              :content      "hi!"}]
            res)))))
