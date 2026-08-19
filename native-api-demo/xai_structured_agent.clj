(ns com.rpl.agent.native-demo.xai-structured-agent
  "ILLUSTRATIVE ONLY — does not compile. Sketches a market-monitoring agent
  on the hypothetical native xAI (Grok) integration.

  What this shows:
  - xAI Live Search: the provider searches the web/X server-side; citations
    come back in the response and land in the trace.
  - Structured output: :output-schema is a plain JSON-schema map; the
    response carries :parsed (already-decoded Clojure data), so no manual
    JSON wrangling and no .strictJsonSchema-style provider quirks leaking
    into user code — the adapter owns those details.
  - Streaming works the same way as every other provider (:stream? true)."
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as m]
   [com.rpl.agent-o-rama.model.xai :as xai]
   [com.rpl.agent-o-rama.schema :as s]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; Plain data schema — same helpers for tool params and structured output
(def SentimentReport
  (s/object
   {"ticker"      (s/string "The ticker symbol analyzed")
    "sentiment"   (s/enum "Overall sentiment" ["bullish" "bearish" "neutral"])
    "confidence"  (s/number "Confidence 0.0-1.0")
    "key_events"  (s/array (s/string "A recent event driving sentiment"))
    "summary"     (s/string "Two-sentence summary")}
   {:description "Market sentiment analysis"
    :required    ["ticker" "sentiment" "confidence" "summary"]}))

(aor/defagentmodule XaiMarketModule
  [topology]

  (xai/declare-model
   topology
   "grok"
   {:api-key "XAI_API_KEY from env"
    :model   "grok-4-1"
    :stream? true
    ;; Live Search runs server-side; :auto lets the model decide when to
    ;; search. Sources include X itself — something no other provider has.
    :search-parameters {:mode    :auto
                        :sources [{:type :web} {:type :x}]}})

  (->
    topology
    (aor/new-agent "MarketSentimentAgent")

    ;; two-node graph: analyze (structured) -> decide (plain chat)
    (aor/node
     "analyze"
     "decide"
     (fn [agent-node ^String ticker]
       (let [grok (aor/get-agent-object agent-node "grok")
             {:keys [parsed provider]}
             (m/chat grok
                     {:messages      [(m/system "You are a market analyst.")
                                      (m/user (str "Analyze current sentiment for " ticker))]
                      ;; adapter sets response_format/strict details correctly
                      ;; per provider; user code just supplies the schema
                      :output-schema SentimentReport})]
         ;; parsed is already a Clojure map matching SentimentReport;
         ;; Live Search citations are in (:citations provider) and the trace
         (aor/emit! agent-node "decide" parsed))))

    (aor/node
     "decide"
     nil
     (fn [agent-node {:strs [ticker sentiment confidence] :as report}]
       (let [grok (aor/get-agent-object agent-node "grok")]
         (if (and (= "bullish" sentiment) (> confidence 0.8))
           ;; streaming chat: deltas auto-emit from this node too
           (let [{:keys [text]}
                 (m/chat grok
                         {:messages [(m/user (str "Draft a one-paragraph alert for "
                                                  ticker " based on: " report))]})]
             (aor/result! agent-node {:action :alert :message text :report report}))
           (aor/result! agent-node {:action :hold :report report})))))))

(defn run-agent
  []
  (with-open [ipc (rtest/create-ipc)
              _   (aor/start-ui ipc)]
    (rtest/launch-module! ipc XaiMarketModule {:tasks 4 :threads 2})
    (let [manager (aor/agent-manager ipc (rama/get-module-name XaiMarketModule))
          agent   (aor/agent-client manager "MarketSentimentAgent")]
      (aor/agent-invoke agent "TSLA"))))
