(ns com.rpl.model-test
  (:use [clojure.test]
        [com.rpl.test-helpers]
        [com.rpl.rama]
        [com.rpl.rama.path])
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.impl.pobjects :as po]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama.test :as rtest])
  (:import
   [com.rpl.agentorama
    IUnderlying]))

(defn scripted-provider
  "Fake ChatProvider returning canned responses in order. A response entry
  may be a Throwable (thrown), or a response map optionally containing
  :deltas, a vector of streaming events emitted via on-delta before the
  response is returned by -stream-chat."
  [info responses]
  (let [idx (atom -1)
        next-response! (fn [] (nth responses (swap! idx inc)))]
    (reify
     m/ChatProvider
     (-provider-info [this] info)
     (-chat [this _request]
       (let [r (next-response!)]
         (if (instance? Throwable r)
           (throw r)
           (dissoc r :deltas))))
     (-stream-chat [this _request on-delta]
       (let [r (next-response!)]
         (if (instance? Throwable r)
           (throw r)
           (do
             (doseq [d (:deltas r)]
               (on-delta d))
             (dissoc r :deltas))))))))

(defn- nested-ops
  [ipc module-name agent-name {:keys [task-id agent-invoke-id]}]
  (let [root-pstate (foreign-pstate ipc
                                    module-name
                                    (po/agent-root-task-global-name agent-name))
        node-pstate (foreign-pstate ipc
                                    module-name
                                    (po/agent-node-task-global-name agent-name))
        root-id     (foreign-select-one [(keypath agent-invoke-id)
                                         :root-invoke-id]
                                        root-pstate
                                        {:pkey task-id})]
    (foreign-select-one [(keypath root-id) :nested-ops]
                        node-pstate
                        {:pkey task-id})))

(deftest chat-provider-basic-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind response
       {:message       (m/assistant "The answer is 42.")
        :text          "The answer is 42."
        :finish-reason :stop
        :usage         {:input-tokens 7 :output-tokens 5 :total-tokens 12}
        :model         "fake-1"})
     (bind module
       (aor/agentmodule
        [topology]
        (aor/declare-agent-object-builder
         topology
         "fake"
         (fn [_setup]
           (scripted-provider {:provider :fake :model "fake-1"} [response])))
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node ^String prompt]
             (let [model      (aor/get-agent-object agent-node "fake")
                   underlying (.getUnderlying ^IUnderlying model)
                   res        (m/chat model
                                      {:messages    [(m/system "You are terse.")
                                                     (m/user prompt)]
                                       :temperature 0.3})]
               (aor/result! agent-node
                            {:text (:text res)
                             :message (:message res)
                             :underlying-provider?
                             (satisfies? m/ChatProvider underlying)})))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     (bind inv (aor/agent-initiate foo "what is 6*7?"))
     (bind res (aor/agent-result foo inv))
     (is (= "The answer is 42." (:text res)))
     (is (= (m/assistant "The answer is 42.") (:message res)))
     (is (:underlying-provider? res))

     (bind [op :as ops] (nested-ops ipc module-name "foo" inv))
     (is (= 1 (count ops)))
     (is (= {"objectName"       "fake"
             "provider"         "fake"
             "modelName"        "fake-1"
             "temperature"      0.3
             "input"            [{"type" "system"
                                  "text" "You are terse."}
                                 {"type"     "user"
                                  "contents" [{"type" "text"
                                               "text" "what is 6*7?"}]}]
             "response"         "The answer is 42."
             "finishReason"     "stop"
             "inputTokenCount"  7
             "outputTokenCount" 5
             "totalTokenCount"  12}
            (:info op)))
    )))

(deftest chat-provider-streaming-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind response
       {:message       (m/assistant "Hello world")
        :text          "Hello world"
        :finish-reason :stop
        :deltas        ["Hel"
                        "lo "
                        {:type :text-delta :text "world"}
                        {:type :reasoning-delta :text "not streamed"}]})
     (bind module
       (aor/agentmodule
        [topology]
        (aor/declare-agent-object-builder
         topology
         "fake"
         (fn [_setup]
           (scripted-provider {:provider :fake :model "fake-1" :stream? true}
                              [response])))
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node ^String prompt]
             (let [model (aor/get-agent-object agent-node "fake")]
               (aor/result! agent-node (:text (m/chat model prompt)))))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     (bind inv (aor/agent-initiate foo "hi"))
     (bind done (promise))
     (aor/agent-stream
      foo
      inv
      "start"
      (fn [all-chunks _new-chunks _reset? complete?]
        (when complete?
          (deliver done all-chunks))))
     (bind res (aor/agent-result foo inv))
     (is (= "Hello world" res))
     ;; text deltas are auto-streamed; the reasoning delta is not
     (is (= ["Hel" "lo " "world"] (deref done 10000 :timeout)))

     (bind [op :as ops] (nested-ops ipc module-name "foo" inv))
     (is (= 1 (count ops)))
     (is (number? (get (:info op) "firstTokenTimeMillis")))
     (is (= "Hello world" (get (:info op) "response")))
    )))

(deftest chat-provider-request-stream-override-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind response
       {:message       (m/assistant "ab")
        :text          "ab"
        :finish-reason :stop
        :deltas        ["a" {:type :reasoning-delta :text "r"} "b"]})
     (bind module
       (aor/agentmodule
        [topology]
        (aor/declare-agent-object-builder
         topology
         "fake"
         (fn [_setup]
           ;; provider defaults to non-streaming
           (scripted-provider {:provider :fake :model "fake-1" :stream? false}
                              [response])))
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node ^String prompt]
             (let [model  (aor/get-agent-object agent-node "fake")
                   events (atom [])
                   res    (m/chat model
                                  {:messages [(m/user prompt)]
                                   :stream?  true}
                                  (fn [delta] (swap! events conj delta)))]
               (aor/result! agent-node
                            {:text   (:text res)
                             :events @events})))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     (bind inv (aor/agent-initiate foo "hi"))
     (bind done (promise))
     (aor/agent-stream
      foo
      inv
      "start"
      (fn [all-chunks _new-chunks _reset? complete?]
        (when complete?
          (deliver done all-chunks))))
     (bind res (aor/agent-result foo inv))
     (is (= "ab" (:text res)))
     ;; the caller-supplied handler sees every event, including non-text ones
     (is (= ["a" {:type :reasoning-delta :text "r"} "b"] (:events res)))
     ;; the streaming depot only receives text
     (is (= ["a" "b"] (deref done 10000 :timeout)))
    )))

(deftest chat-provider-failure-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind module
       (aor/agentmodule
        [topology]
        (aor/declare-agent-object-builder
         topology
         "fake"
         (fn [_setup]
           (scripted-provider {:provider :fake :model "fake-1"}
                              [(ex-info "model boom" {})])))
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node ^String prompt]
             (let [model (aor/get-agent-object agent-node "fake")]
               (aor/result!
                agent-node
                (try
                  (m/chat model prompt)
                  "no-throw"
                  (catch Exception e (.getMessage e))))))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     (bind inv (aor/agent-initiate foo "hi"))
     (bind res (aor/agent-result foo inv))
     (is (= "model boom" res))

     (bind [op :as ops] (nested-ops ipc module-name "foo" inv))
     (is (= 1 (count ops)))
     (is (= "fake" (get (:info op) "objectName")))
     (is (= [{"type"     "user"
              "contents" [{"type" "text" "text" "hi"}]}]
            (get (:info op) "input")))
     (is (str/includes? (get (:info op) "failure") "model boom"))
     (is (nil? (get (:info op) "response")))
    )))

(deftest tool-call-trace-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind response
       {:message       {:role    :assistant
                        :content [{:type :reasoning
                                   :summary "Need to add."}
                                  {:type :tool-call
                                   :id   "c1"
                                   :name "add"
                                   :args {"a" 1 "b" 2}}]}
        :tool-calls    [{:id "c1" :name "add" :args {"a" 1 "b" 2}}]
        :finish-reason :tool-calls
        :usage         {:input-tokens 3 :output-tokens 4 :total-tokens 7}})
     (bind module
       (aor/agentmodule
        [topology]
        (aor/declare-agent-object-builder
         topology
         "fake"
         (fn [_setup]
           (scripted-provider {:provider :fake :model "fake-1"} [response])))
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node ^String prompt]
             (let [model (aor/get-agent-object agent-node "fake")
                   res   (m/chat model prompt)]
               (aor/result! agent-node (:tool-calls res))))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     (bind inv (aor/agent-initiate foo "add 1 and 2"))
     (bind res (aor/agent-result foo inv))
     (is (= [{:id "c1" :name "add" :args {"a" 1 "b" 2}}] res))

     (bind [op :as ops] (nested-ops ipc module-name "foo" inv))
     (is (= 1 (count ops)))
     (is (= [{"id"       "c1"
              "toolName" "add"
              "args"     {"a" 1 "b" 2}}]
            (get (:info op) "toolRequests")))
     (is (= "tool-calls" (get (:info op) "finishReason")))
    )))

(deftest native-tools-agent-test
  (with-open [ipc (rtest/create-ipc)]
    (letlocals
     (bind add-tool
       (tools/tool
        {:name        "add"
         :description "Add two numbers"
         :schema      (schema/object
                       {:required ["a" "b"]}
                       {"a" (schema/number "first number")
                        "b" (schema/number "second number")})}
        (fn [args] (+ (get args "a") (get args "b")))))
     (bind context-tool
       (tools/tool
        {:name        "greet"
         :description "Greet using agent context"
         :schema      (schema/object {"who" (schema/string)})}
        (fn [agent-node _caller-data args]
          (str "hello " (get args "who")))
        {:include-context? true}))
     (bind module
       (aor/agentmodule
        [topology]
        (tools/new-tools-agent topology "tools" [add-tool context-tool])
        (->
          topology
          (aor/new-agent "foo")
          (aor/node
           "start"
           nil
           (fn [agent-node tool-calls]
             (let [tools-agent (aor/agent-client agent-node "tools")]
               (aor/result! agent-node
                            (aor/agent-invoke tools-agent tool-calls))))))))
     (launch-module-without-eval-agent! ipc module {:tasks 4 :threads 2})
     (bind module-name (get-module-name module))
     (bind agent-manager (aor/agent-manager ipc module-name))
     (bind foo (aor/agent-client agent-manager "foo"))

     ;; neutral tool-call maps in (a response's :tool-calls), neutral
     ;; {:role :tool ...} messages out
     (bind res
       (sort-by :tool-call-id
                (aor/agent-invoke
                 foo
                 [{:id "c1" :name "add" :args {"a" 1 "b" 2}}
                  {:id "c2" :name "greet" :args {"who" "world"}}
                  {:id "c3" :name "missing" :args {}}])))
     (is (= [{:role         :tool
              :tool-call-id "c1"
              :name         "add"
              :content      "3"}
             {:role         :tool
              :tool-call-id "c2"
              :name         "greet"
              :content      "hello world"}]
            (take 2 res)))
     (bind {:keys [content] :as invalid-res} (nth res 2))
     (is (= "c3" (:tool-call-id invalid-res)))
     (is (str/includes? content "missing is not a valid tool"))
     (is (str/includes? content "add"))
    )))
