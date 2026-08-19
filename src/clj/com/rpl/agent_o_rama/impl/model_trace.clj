(ns com.rpl.agent-o-rama.impl.model-trace
  "Converts provider-neutral messages/responses (com.rpl.agent-o-rama.model)
  to the string-keyed trace maps stored in nested ops and rendered by the
  trace UI."
  (:require
   [com.rpl.agent-o-rama.impl.helpers :as h]
   [com.rpl.agent-o-rama.model :as model]))

(defn tool-call->trace
  [{:keys [id name args]}]
  {"id"       id
   "toolName" name
   "args"     args})

(defn tool-calls->trace
  [tool-calls]
  (mapv tool-call->trace tool-calls))

(defn- content-block->trace
  [block]
  (h/remove-empty-vals
   (case (:type block)
     :text      {"type" "text"
                 "text" (:text block)}
     :reasoning {"type"    "reasoning"
                 "summary" (:summary block)}
     :tool-call {"type"     "toolCall"
                 "id"       (:id block)
                 "toolName" (:name block)
                 "args"     (:args block)}
     :image     {"type"       "image"
                 "mimeType"   (:mime-type block)
                 "url"        (:url block)
                 "dataLength" (some-> (:data block) count)}
     :audio     {"type"       "audio"
                 "mimeType"   (:mime-type block)
                 "url"        (:url block)
                 "dataLength" (some-> (:data block) count)}
     :pdf       {"type"       "pdf"
                 "mimeType"   (:mime-type block)
                 "url"        (:url block)
                 "dataLength" (some-> (:data block) count)}
     {"type" "unknown"
      "str"  (pr-str block)})))

(defn message->trace
  [{:keys [role content] :as message}]
  (h/remove-empty-vals
   (case role
     :system    {"type" "system"
                 "text" (model/content-text content)}
     :user      {"type"     "user"
                 "name"     (:name message)
                 "contents" (mapv content-block->trace
                                  (model/content-blocks content))}
     :assistant (let [blocks (model/content-blocks content)]
                  {"type"         "ai"
                   "text"         (model/content-text content)
                   "reasoning"    (into []
                                        (keep :summary)
                                        (filter #(= :reasoning (:type %))
                                                blocks))
                   "toolRequests" (tool-calls->trace
                                   (filter #(= :tool-call (:type %))
                                           blocks))})
     :tool      {"type"     "toolResult"
                 "id"       (:tool-call-id message)
                 "toolName" (:name message)
                 "text"     (model/content-text content)}
     {"type" "unknown"
      "str"  (pr-str message)})))

(defn messages->trace
  [messages]
  (mapv message->trace messages))

(defn finish-reason->trace
  [finish-reason]
  (some-> finish-reason name))
