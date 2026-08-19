(ns com.rpl.agent-o-rama.impl.model-http
  "Shared HTTP/SSE transport for native model provider integrations, over
  java.net.http (no external dependencies). Blocking calls — agent nodes run
  on virtual threads."
  (:require
   [clojure.string :as str]
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [jsonista.core :as j])
  (:import
   [java.net URI]
   [java.net.http
    HttpClient
    HttpRequest
    HttpRequest$BodyPublishers
    HttpResponse
    HttpResponse$BodyHandlers]
   [java.time Duration]
   [java.util.stream Stream]))

(def MAPPER (j/object-mapper {:decode-key-fn str}))

(defn json-encode
  ^String [x]
  (j/write-value-as-string x))

(defn json-decode
  [^String s]
  (j/read-value s MAPPER))

(defn mk-client
  ^HttpClient [{:keys [connect-timeout-ms]
                :or   {connect-timeout-ms 10000}}]
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofMillis connect-timeout-ms))
      .build))

(defn close-client!
  [client]
  (when (instance? java.lang.AutoCloseable client)
    (.close ^java.lang.AutoCloseable client)))

(defn- http-request
  ^HttpRequest [{:keys [url headers timeout-ms]
                 :or   {timeout-ms 600000}}
                body]
  (let [builder (-> (HttpRequest/newBuilder (URI/create url))
                    (.timeout (Duration/ofMillis timeout-ms))
                    (.header "Content-Type" "application/json"))]
    (doseq [[k v] headers]
      (.header builder k v))
    (-> builder
        (.POST (HttpRequest$BodyPublishers/ofString (json-encode body)))
        .build)))

(defn request-failed
  [provider status body]
  (h/ex-info (str (name provider) " request failed")
             {:provider provider
              :status   status
              :error    (try
                          (get (json-decode body) "error")
                          (catch Exception _ body))}))

(defn post-json
  "POSTs body as JSON and returns the decoded (string-keyed) response map.
  Throws on non-2xx."
  [^HttpClient client {:keys [provider] :as endpoint} body]
  (let [^HttpResponse resp (.send client
                                  (http-request endpoint body)
                                  (HttpResponse$BodyHandlers/ofString))]
    (if (<= 200 (.statusCode resp) 299)
      (json-decode (.body resp))
      (throw (request-failed provider (.statusCode resp) (.body resp))))))

(defn post-json-sse
  "POSTs body as JSON and passes the seq of response lines (an SSE stream)
  to lines-fn, returning its result. Throws on non-2xx."
  [^HttpClient client {:keys [provider] :as endpoint} body lines-fn]
  (let [^HttpResponse resp (.send client
                                  (http-request endpoint body)
                                  (HttpResponse$BodyHandlers/ofLines))
        lines (iterator-seq (.iterator ^Stream (.body resp)))]
    (if (<= 200 (.statusCode resp) 299)
      (lines-fn lines)
      (throw (request-failed provider
                             (.statusCode resp)
                             (str/join "\n" lines))))))

(defn each-sse-payload
  "Calls (f payload) for each SSE data payload in lines, decoded as JSON
  (string keys), as it arrives. Skips [DONE] terminators and non-data lines."
  [lines f]
  (doseq [^String line lines]
    (when (str/starts-with? line "data:")
      (let [payload (str/trim (subs line 5))]
        (when-not (= "[DONE]" payload)
          (f (json-decode payload)))))))
