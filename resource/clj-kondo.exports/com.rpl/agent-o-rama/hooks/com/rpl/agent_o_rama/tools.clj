(ns hooks.com.rpl.agent-o-rama.tools
  "clj-kondo hooks for com.rpl.agent-o-rama.tools."
  (:require
   [clj-kondo.hooks-api :as api]))

(defn- map-node-get
  "Value node for key k in a map node, or nil."
  [map-node k]
  (when map-node
    (some (fn [[key-node value-node]]
            (when (= k (api/sexpr key-node)) value-node))
          (partition 2 (:children map-node)))))

(defn deftool
  "Rewrites

    (deftool name docstring? options? [p schema ...] body)

  into

    (def name docstring? (fn [p ... context ... as] (let [_ [schema ...]] body)))

  so that parameter, :context and :as bindings resolve in the body, and the
  schema expressions are still linted."
  [{:keys [node]}]
  (let [[name-node & more] (rest (:children node))
        [doc-node more]    (if (and (seq more) (api/string-node? (first more)))
                             [(first more) (rest more)]
                             [nil more])
        [opts-node more]   (if (and (seq more) (api/map-node? (first more)))
                             [(first more) (rest more)]
                             [nil more])
        params-node        (first more)
        body               (rest more)]
    (when (and name-node (api/vector-node? params-node))
      (let [pairs        (partition-all 2 (:children params-node))
            param-syms   (mapv first pairs)
            schemas      (vec (keep second pairs))
            context-syms (when-let [c (map-node-get opts-node :context)]
                           (when (api/vector-node? c) (vec (:children c))))
            as-sym       (when-let [a (map-node-get opts-node :as)]
                           (when (api/token-node? a) a))
            bindings     (vec (concat param-syms
                                      context-syms
                                      (when as-sym [as-sym])))]
        {:node
         (api/list-node
          (concat
           [(api/token-node 'def) name-node]
           (when doc-node [doc-node])
           [(api/list-node
             (list
              (api/token-node 'fn)
              (api/vector-node bindings)
              (api/list-node
               (list*
                (api/token-node 'let)
                (api/vector-node [(api/token-node '_)
                                  (api/vector-node schemas)])
                body))))]))}))))
