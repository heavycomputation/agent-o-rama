(ns com.rpl.json-serialize-test
  (:use [clojure.test])
  (:require
   [com.rpl.agent-o-rama.impl.json-serialize :as jser]))

(deftest json-freeze-thaw-test
  ;; plain Clojure data (including neutral message maps) round-trips as JSON
  (let [obj [{"role"    "assistant"
              "content" [{"type" "text" "text" "abc"}
                         {"type" "tool-call" "id" "c1" "name" "add"
                          "args" {"a" 1 "b" 2}}]}
             {"a" ["x" "y"]
              "b" 45
              "c" "some data"
              "d" 1.5
              "e" true
              "f" nil}]
        json (jser/json-freeze obj)]
    (is (string? json))
    (is (= obj (jser/json-thaw json)))))

(deftest unhandled-aor-type-test
  ;; unknown _aor-type tags fail loudly on thaw
  (is (thrown-with-msg?
       Exception #"No deserializer found"
       (jser/json-thaw "{\"_aor-type\": \"com.example.Gone\"}"))))

(deftest unserializable-falls-back-to-str-test
  ;; values jsonista can't encode degrade to their string form
  (let [json (jser/json-freeze {"obj" (Object.)})]
    (is (string? json))
    (is (.contains ^String json "java.lang.Object@"))))
