;; Modified by Heavy Computation in 2026 as part of its Agent-o-rama fork.
(ns com.rpl.agent-o-rama.ui.components.conversation
  "Components and utilities for displaying conversation data (neutral
  message maps: {:role :system|:user|:assistant|:tool :content ...})"
  (:require
   [re-frame.core :as rf]
   [clojure.string :as str]
   [uix.core :refer [defui $]]))

(defn get-flexible
  "Get a value from a map using either a string or keyword key"
  [m k]
  (or (get m k)
      (get m (keyword k))))

(defn- ->name
  [x]
  (cond
    (keyword? x) (name x)
    (string? x)  x
    :else        nil))

(def ^:private ROLES #{"system" "user" "assistant" "tool"})

(defn chat-message?
  "Check if a map represents a chat message ({:role ... :content ...}).
  Handles string and keyword keys/values."
  [m]
  (boolean
   (and (map? m)
        (contains? ROLES (->name (get-flexible m "role"))))))

(defn conversation?
  "Check if data is a conversation (vector of chat messages and/or strings).
  At least one element must be a chat message; all elements must be either
  strings or chat messages."
  [data]
  (boolean
   (and (sequential? data)
        (seq data)
        (some chat-message? data)
        (every? #(or (string? %) (chat-message? %)) data))))

(defn- block-text
  "Render a single content block as display text."
  [block]
  (case (->name (get-flexible block "type"))
    "text"      (get-flexible block "text")
    "reasoning" (let [summary (get-flexible block "summary")]
                  (str "💭 Reasoning"
                       (when-not (str/blank? summary)
                         (str ": " summary))))
    "tool-call" (str "🔧 Tool call: " (get-flexible block "name")
                     (when-let [args (get-flexible block "args")]
                       (str "\nArguments: " (js/JSON.stringify
                                             (clj->js args))))
                     (when-let [id (get-flexible block "id")]
                       (str "\nID: " id)))
    nil))

(defn- content-text
  "Render message content (a string or a vector of blocks) as display text."
  [content separator]
  (cond
    (string? content)     content
    (sequential? content) (->> content
                               (map block-text)
                               (filter some?)
                               (str/join separator))
    :else                 nil))

(defn extract-message-role-and-text
  "Extract role and text from a chat message or string.
  Returns a map with :role (\"system\"/\"user\"/\"assistant\"/\"tool\") and
  :text keys. Optional separator controls how content blocks are joined
  (default: newline)."
  ([msg] (extract-message-role-and-text msg "\n"))
  ([msg separator]
   (if (string? msg)
     {:role "user"
      :text msg}
     (let [role (->name (get-flexible msg "role"))
           text (content-text (get-flexible msg "content") separator)]
       (if (= "tool" role)
         (let [tool-name (->name (get-flexible msg "name"))
               tool-id   (get-flexible msg "tool-call-id")
               result    (or text "")]
           {:role role
            :text (str (when tool-name (str "🔧 " tool-name " result"))
                       (when (and tool-name tool-id)
                         (str " (ID: " tool-id ")"))
                       (when (and (or tool-name tool-id)
                                  (not (str/blank? result)))
                         (str "\n" result))
                       (when (and (not tool-name) (not tool-id))
                         result))})
         {:role role
          :text text})))))

(def ^:private ROLE-LABELS
  {"system"    "SYSTEM"
   "user"      "USER"
   "assistant" "AI"
   "tool"      "TOOL"})

(defn conversation-preview-text
  "Generate preview text for a conversation.
  Returns a vector of preview lines (not truncated)."
  [messages]
  (let [preview-lines
        (->> messages
             (take 3)
             (mapv (fn [msg]
                     (let [{:keys [role text]} (extract-message-role-and-text
                                                msg " ")
                           label (get ROLE-LABELS role (or role "MSG"))
                           display-text (if (str/blank? text)
                                          "(empty)"
                                          text)]
                       (str label ": " display-text)))))]
    (if (> (count messages) 3)
      (conj
       preview-lines
       (str "... (" (- (count messages) 3) " more messages)"))
      preview-lines)))

(defui ConversationModal [{:keys [messages]}]
  ($ :div {:className "p-6 space-y-4 max-h-[600px] overflow-y-auto"}
     (for [[idx msg] (map-indexed vector messages)]
       (let [{:keys [role text]} (extract-message-role-and-text msg)
             [bg-class text-class label]
             (case role
               "system"    ["bg-gray-100" "text-gray-700" "SYSTEM"]
               "user"      ["bg-blue-50" "text-blue-900" "USER"]
               "assistant" ["bg-green-50" "text-green-900" "AI"]
               "tool"      ["bg-purple-50" "text-purple-900" "TOOL RESULT"]
               ["bg-gray-50" "text-gray-800" (or role "MESSAGE")])]
         ($ :div
            {:key idx
             :className (str bg-class " p-3 rounded-lg border border-gray-200")}
            ($ :div {:className "flex items-center gap-2 mb-2"}
               ($ :span
                  {:className (str "text-xs font-bold " text-class " uppercase tracking-wide")}
                  label))
            ($ :pre
               {:className (str "text-sm " text-class " whitespace-pre-wrap break-words font-sans")
                :style {:overflow-wrap "break-word"
                        :word-break "break-word"}}
               (or text "(no text)")))))))

(defui conversation-display
  "Display a compact preview of a conversation with click to expand.
  preview-text should be a vector of lines to display."
  [{:keys [messages color preview-text]
    :or {color "blue"}}]
  (let [num-messages (count messages)
        display-modal
        (fn [e]
          (.stopPropagation e)
          (rf/dispatch
           [:modal/show :conversation
            {:title (str "Conversation (" num-messages " messages)")
             :component
             ($ ConversationModal
                {:title (str "Conversation (" num-messages " messages)")
                 :messages messages})}]))
        display-json-modal
        (fn [e]
          (.stopPropagation e)
          (let [json-str (js/JSON.stringify (clj->js messages) nil 2)]
            (rf/dispatch
             [:modal/show :conversation-json
              {:title "Conversation (JSON)"
               :component
               ($ :div.p-6
                  ($ :pre.text-xs.bg-gray-50.p-3.rounded.border.overflow-y-auto.max-h-96.font-mono.whitespace-pre-wrap.break-words
                     json-str))}])))]
    ($ :div
       {:className
        (str "text-" color "-600 bg-" color "-50 border border-" color "-200 rounded p-2 min-w-0 max-w-full overflow-hidden")}
       ($ :div {:className "flex items-center justify-between text-xs font-semibold mb-1 min-w-0 gap-2"
                :style {:color "#6b7280"}}
          ($ :span {:className "truncate"} (str "💬 Conversation (" num-messages " messages)"))
          ($ :a {:className "text-blue-600 hover:text-blue-800 underline cursor-pointer flex-shrink-0"
                 :onClick display-json-modal
                 :title "View as JSON"}
             "as json"))
       ($ :div
          {:className
           (str "cursor-pointer hover:bg-" color "-100 px-1 py-0.5 rounded transition-colors min-w-0 overflow-hidden")
           :onClick display-modal
           :title "Click to view full conversation"}
          ($ :div {:className "text-xs font-sans space-y-0.5 min-w-0 max-w-full"}
             (for [[idx line] (map-indexed vector preview-text)]
               ($ :div {:key idx
                        :className "truncate min-w-0"}
                  line)))))))
