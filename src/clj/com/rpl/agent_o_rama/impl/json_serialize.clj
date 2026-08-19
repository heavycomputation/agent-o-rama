(ns com.rpl.agent-o-rama.impl.json-serialize
  (:use [com.rpl.rama path])
  (:require
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [jsonista.core :as j])
  (:import
   [java.util
    List
    Map]))

(def MAPPER (j/object-mapper {:decode-key-fn str}))

(defprotocol JSONFreeze
  (json-freeze* [this]))

(defn json-freeze*-with-type
  [x]
  (let [m (json-freeze* x)]
    (when-not (map? m)
      (throw (ex-info "json-freeze* must return a map"
                      {:value x :returned m})))
    (assoc
     (setval [MAP-VALS nil?] NONE m)
     "_aor-type"
     (-> x
         class
         .getName))))

(defn- freeze-walk
  [x]
  (if (satisfies? JSONFreeze x)
    (json-freeze*-with-type x)
    (cond
      (instance? Map x)
      (transform MAP-VALS freeze-walk (into {} x))

      (instance? List x)
      (transform ALL freeze-walk (into [] x))

      :else
      (try
        (j/write-value-as-string x MAPPER)
        x

        (catch Throwable t
          (str x))))))

(defn json-freeze
  ^String [obj]
  (j/write-value-as-string (freeze-walk obj) MAPPER))

(defmulti json-thaw*
  (fn [obj]
    (if (and (map? obj) (contains? obj "_aor-type"))
      (get obj "_aor-type")
    )))

(defmethod json-thaw* :default
  [obj]
  (if (and (map? obj) (contains? obj "_aor-type"))
    (throw (h/ex-info "No deserializer found for AOR type"
                      {:aor-type (get obj "_aor-type")}))
    obj))

(defn walk-json-thaw*
  [obj]
  (let [obj2 (json-thaw* obj)]
    (if-not (identical? obj obj2)
      obj2
      (cond (map? obj)
            (transform MAP-VALS walk-json-thaw* obj)

            (sequential? obj)
            (transform ALL walk-json-thaw* obj)

            :else
            obj
      ))))

(defn json-thaw
  [str]
  (let [obj (j/read-value str MAPPER)]
    (walk-json-thaw* obj)))
