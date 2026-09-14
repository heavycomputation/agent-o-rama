;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent.react
  "This defines a custom reasoning and action agent graph.
  It invokes tools in a simple loop."
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.agent-o-rama.tools :as tools]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]
   [jsonista.core :as json])
  (:import
   [java.net URI]
   [java.net.http
    HttpClient
    HttpRequest
    HttpRequest$BodyPublishers
    HttpResponse$BodyHandlers]))

(defn- tavily-search
  "Performs a Tavily web search and returns the result contents joined
  with separators."
  [api-key terms max-results]
  (let [body     (json/write-value-as-string
                  {"api_key"         api-key
                   "query"           terms
                   "max_results"     max-results
                   "exclude_domains" ["en.wikipedia.org"]})
        request  (-> (HttpRequest/newBuilder)
                     (.uri (URI/create "https://api.tavily.com/search"))
                     (.header "Content-Type" "application/json")
                     (.POST (HttpRequest$BodyPublishers/ofString body))
                     .build)
        client   (HttpClient/newHttpClient)
        response (.send client request (HttpResponse$BodyHandlers/ofString))
        parsed   (json/read-value (.body response))]
    (str/join
     "\n---\n"
     (mapv #(get % "content") (get parsed "results")))))

(def ^:private TOOLS
  "Description of available tools"
  [(tools/tool
    {:name        "tavily"
     :description "Search the web"
     :schema      (schema/object
                   {:description "Map containing the terms to search for"
                    :required    ["terms"]}
                   {"terms" (schema/string "The terms to search for")})}
    (fn [agent-node _ arguments]
      (let [terms   (get arguments "terms")
            api-key (aor/get-agent-object agent-node "tavily-api-key")]
        (tavily-search api-key terms 3)))
    {:include-context? true})])

(aor/defagentmodule ReActModule
  [topology]
  (aor/declare-agent-object
   topology
   "tavily-api-key"
   (System/getenv "TAVILY_API_KEY"))
  (openai/declare-model
   topology
   "openai"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"})
  (tools/new-tools-agent topology "tools" TOOLS)
  (->
    topology
    (aor/new-agent "ReActAgent")
    (aor/node
     "chat"
     "chat"
     (fn [agent-node messages]
       (let [openai (aor/get-agent-object agent-node "openai")
             tools  (aor/agent-client agent-node "tools")
             {:keys [message tool-calls text]}
             (model/chat openai {:messages messages :tools TOOLS})]
         (if (seq tool-calls)
           (let [tool-results  (aor/agent-invoke tools tool-calls)
                 next-messages (into (conj messages message) tool-results)]
             (aor/emit! agent-node "chat" next-messages))
           (aor/result! agent-node text)))))))

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              _ (aor/start-ui ipc)]
    (rtest/launch-module! ipc ReActModule {:tasks 4 :threads 2})
    (let [module-name   (rama/get-module-name ReActModule)
          agent-manager (aor/agent-manager ipc module-name)
          agent         (aor/agent-client agent-manager "ReActAgent")
          _ (print "Ask your question (agent has web search access): ")
          _ (flush)
          ^String user-input (read-line)
          result        (aor/agent-invoke agent [(model/user user-input)])]
      (println result))))
