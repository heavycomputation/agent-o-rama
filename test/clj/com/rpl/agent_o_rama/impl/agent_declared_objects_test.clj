;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent-o-rama.impl.agent-declared-objects-test
  (:require [clojure.test :refer :all])
  (:import [com.rpl.agentorama AgentClient AgentManager]
           [com.rpl.agentorama.impl AgentDeclaredObjectsTaskGlobal]
           [com.rpl.rama ModuleInstanceInfo]
           [com.rpl.rama.integration ManagedResource TaskGlobalContext
            WorkerManagedResource]
           [java.util Arrays UUID]
           [java.util.concurrent Callable CountDownLatch Executors Future TimeUnit]))

(defn- ^java.lang.reflect.Field accessible-field
  [^Class cls name]
  (doto (.getDeclaredField cls name) (.setAccessible true)))

(defn- colliding-task-global
  [scope manager-constructor]
  (let [info (ModuleInstanceInfo. "test-module" scope 1 1 scope 1234 #{0})
        context (reify TaskGlobalContext
                  (getModuleInstanceInfo [_] info)
                  (getClusterRetriever [_] nil))
        global (AgentDeclaredObjectsTaskGlobal. {} {} {} {"tools" nil})]
    (.prepareForTask global 0 context)
    ;; Preserve the production client-map constructor and Rama's getResource/cache.
    ;; Stub manager creation to avoid needing a cluster. Aa and BB have equal
    ;; String hash codes, so these keys collide at every cache table capacity.
    ;; A unique scope isolates each fixture without clearing Rama's shared cache.
    (doseq [[field-name key] [["_thisManager" "Aa"] ["_agents" "BB"]]]
      (let [field (accessible-field AgentDeclaredObjectsTaskGlobal field-name)
            resource (.get field global)
            constructor (if (= field-name "_thisManager")
                          manager-constructor
                          (.get (accessible-field ManagedResource "_resourceConstructor")
                                resource))
            id (Arrays/asList (object-array [scope key]))]
        (.set field global
              (proxy [WorkerManagedResource] [key context constructor]
                (idTuple [_] id)))))
    global))

(defn- with-client-fixture
  [f]
  (let [scope (str (UUID/randomUUID))
        builds (atom {:manager 0 :client 0})
        closes (atom {:manager 0 :client 0})
        globals (atom [])
        client (reify AgentClient
                 (close [_] (swap! closes update :client inc)))
        manager (reify AgentManager
                  (getAgentClient [_ name]
                    (is (= "tools" name))
                    (swap! builds update :client inc)
                    client)
                  (close [_] (swap! closes update :manager inc)))]
    (try
      (f {:new-global #(let [global (colliding-task-global
                                    scope
                                    (fn []
                                      (swap! builds update :manager inc)
                                      manager))]
                        (swap! globals conj global)
                        global)
          :client client
          :builds builds})
      (finally
        (doseq [^AgentDeclaredObjectsTaskGlobal global @globals]
          (.close global))))
    (is (= {:manager 1 :client 1} @closes))))

(deftest colliding-client-resource-initialization-test
  (is (= (.hashCode "Aa") (.hashCode "BB")))
  (with-client-fixture
    (fn [{:keys [new-global client builds]}]
      (let [^AgentDeclaredObjectsTaskGlobal global (new-global)]
        (is (= {:manager 0 :client 0} @builds) "Preparation remains lazy")
        (is (identical? client (.getAgentClient global "tools")))
        (is (identical? client (.getAgentClient global "tools")))
        (is (thrown-with-msg? RuntimeException
                              #"Tried to fetch non-existent agent: missing"
                              (.getAgentClient global "missing")))
        (is (= {:manager 1 :client 1} @builds))))))

(deftest concurrent-colliding-client-resource-initialization-test
  (dotimes [_ 10]
    (with-client-fixture
      (fn [{:keys [new-global client builds]}]
        (let [n 8
              ;; Separate task globals share the worker's resource keys, just
              ;; as in production, but have separate ManagedResource wrappers.
              globals (vec (repeatedly n new-global))
              ready (CountDownLatch. n)
              start (CountDownLatch. 1)
              pool (Executors/newFixedThreadPool n)]
          (try
            (let [calls (mapv (fn [^AgentDeclaredObjectsTaskGlobal global]
                                (let [^Callable task
                                      (bound-fn []
                                        (.countDown ready)
                                        (when-not (.await start 10 TimeUnit/SECONDS)
                                          (throw (ex-info "Start gate timed out" {})))
                                        (.getAgentClient global "tools"))]
                                  (.submit pool task)))
                              globals)]
              (is (.await ready 10 TimeUnit/SECONDS))
              (.countDown start)
              (doseq [^Future call calls]
                (is (identical? client (.get call 10 TimeUnit/SECONDS))))
              (is (= {:manager 1 :client 1} @builds)))
            (finally
              (.countDown start)
              (.shutdownNow pool)
              (is (.awaitTermination pool 10 TimeUnit/SECONDS)))))))))
