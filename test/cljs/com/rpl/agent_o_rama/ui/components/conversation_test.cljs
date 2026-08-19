(ns com.rpl.agent-o-rama.ui.components.conversation-test
  (:require
   [cljs.test :refer [deftest is testing]]
   [clojure.string :as str]
   [com.rpl.agent-o-rama.ui.components.conversation :as conversation]))

(deftest chat-message-test
  (testing "chat-message? detects neutral message maps"
    (testing "recognizes messages with keyword keys/values"
      (is (conversation/chat-message?
           {:role :system :content "You are a helpful assistant"}))
      (is (conversation/chat-message? {:role :user :content "Hello"}))
      (is (conversation/chat-message? {:role :assistant :content "Hi there!"}))
      (is (conversation/chat-message?
           {:role :tool :tool-call-id "c1" :name "add" :content "3"})))

    (testing "recognizes messages with string keys/values (JSON round trip)"
      (is (conversation/chat-message?
           {"role" "system" "content" "You are a helpful assistant"}))
      (is (conversation/chat-message? {"role" "user" "content" "Hello"}))
      (is (conversation/chat-message? {"role" "assistant"
                                       "content" "Hi there!"})))

    (testing "rejects maps with unknown roles"
      (is (false? (conversation/chat-message? {:role :wizard
                                               :content "abracadabra"}))))

    (testing "rejects maps without a role"
      (is (false? (conversation/chat-message? {"content" "no role"}))))

    (testing "rejects non-maps"
      (is (false? (conversation/chat-message? "not a map")))
      (is (false? (conversation/chat-message? nil))))))

(deftest extract-message-role-and-text-test
  (testing "extract-message-role-and-text extracts role and text correctly"
    (testing "system message"
      (let [result (conversation/extract-message-role-and-text
                    {:role :system :content "You are a helpful assistant"})]
        (is (= "system" (:role result)))
        (is (= "You are a helpful assistant" (:text result)))))

    (testing "user message"
      (let [result (conversation/extract-message-role-and-text
                    {:role :user :content "Hello"})]
        (is (= "user" (:role result)))
        (is (= "Hello" (:text result)))))

    (testing "assistant message with content blocks"
      (let [result (conversation/extract-message-role-and-text
                    {:role :assistant
                     :content [{:type :text :text "Part 1"}
                               {:type :text :text "Part 2"}]})]
        (is (= "assistant" (:role result)))
        (is (= "Part 1\nPart 2" (:text result)))))

    (testing "custom separator"
      (let [result (conversation/extract-message-role-and-text
                    {:role :assistant
                     :content [{:type :text :text "Part 1"}
                               {:type :text :text "Part 2"}]}
                    " ")]
        (is (= "Part 1 Part 2" (:text result)))))

    (testing "assistant tool-call blocks render as tool calls"
      (let [result (conversation/extract-message-role-and-text
                    {:role :assistant
                     :content [{:type :tool-call :id "c1" :name "add"
                                :args {"a" 1}}]})]
        (is (str/includes? (:text result) "Tool call: add"))
        (is (str/includes? (:text result) "ID: c1"))))

    (testing "reasoning blocks render with summary"
      (let [result (conversation/extract-message-role-and-text
                    {:role :assistant
                     :content [{:type :reasoning :summary "thinking hard"}
                               {:type :text :text "answer"}]})]
        (is (str/includes? (:text result) "Reasoning: thinking hard"))
        (is (str/includes? (:text result) "answer"))))

    (testing "tool result message includes tool name and id"
      (let [result (conversation/extract-message-role-and-text
                    {:role :tool :tool-call-id "c1" :name "add"
                     :content "3"})]
        (is (= "tool" (:role result)))
        (is (str/includes? (:text result) "add result"))
        (is (str/includes? (:text result) "ID: c1"))
        (is (str/includes? (:text result) "3"))))

    (testing "string-keyed messages work"
      (let [result (conversation/extract-message-role-and-text
                    {"role" "user" "content" "Hello"})]
        (is (= "user" (:role result)))
        (is (= "Hello" (:text result)))))

    (testing "plain strings are treated as user messages"
      (let [result (conversation/extract-message-role-and-text "Hello")]
        (is (= "user" (:role result)))
        (is (= "Hello" (:text result)))))

    (testing "missing content"
      (let [result (conversation/extract-message-role-and-text
                    {:role :user})]
        (is (= "user" (:role result)))
        (is (nil? (:text result)))))))

(deftest conversation-test
  (testing "conversation? detects conversation vectors"
    (testing "recognizes valid conversation"
      (is (conversation/conversation?
           [{:role :system :content "You are helpful"}
            {:role :user :content "Hi"}
            {:role :assistant :content "Hello!"}])))

    (testing "recognizes single message as conversation"
      (is (conversation/conversation? [{:role :user :content "Hi"}])))

    (testing "rejects empty vector"
      (is (not (conversation/conversation? []))))

    (testing "rejects vector with non-messages"
      (is (not (conversation/conversation?
                [{:role :user :content "Hi"}
                 {"some" "other data"}]))))

    (testing "rejects non-sequential data"
      (is (not (conversation/conversation? {:role :user :content "Hi"})))
      (is (not (conversation/conversation? "not a vector")))
      (is (not (conversation/conversation? nil))))))

(deftest conversation-preview-text-test
  (testing "conversation-preview-text generates correct preview"
    (testing "returns a vector of preview lines without truncation"
      (let [messages [{:role :system :content "You are a helpful assistant"}
                      {:role :user
                       :content "Hello, can you help me with something today? I have a question."}
                      {:role :assistant
                       :content "Of course! I'd be happy to help you."}]
            preview  (conversation/conversation-preview-text messages)]
        (is (vector? preview))
        (is (= 3 (count preview)))
        (is (= "SYSTEM: You are a helpful assistant" (nth preview 0)))
        (is (= "USER: Hello, can you help me with something today? I have a question." (nth preview 1)))
        (is (= "AI: Of course! I'd be happy to help you." (nth preview 2)))))

    (testing "indicates when there are more messages"
      (let [messages [{:role :system :content "System"}
                      {:role :user :content "User 1"}
                      {:role :assistant :content "AI 1"}
                      {:role :user :content "User 2"}
                      {:role :assistant :content "AI 2"}]
            preview  (conversation/conversation-preview-text messages)]
        (is (= 4 (count preview)))
        (is (= "... (2 more messages)" (last preview)))))

    (testing "handles messages with content blocks"
      (let [messages [{:role :user
                       :content [{:type :text :text "Part 1"}
                                 {:type :text :text "Part 2"}]}]
            preview  (conversation/conversation-preview-text messages)]
        (is (= ["USER: Part 1 Part 2"] preview))))

    (testing "handles empty text"
      (let [messages [{:role :user :content ""}]
            preview  (conversation/conversation-preview-text messages)]
        (is (= ["USER: (empty)"] preview))))

    (testing "shows exactly 3 messages without more indicator"
      (let [messages [{:role :user :content "1"}
                      {:role :assistant :content "2"}
                      {:role :user :content "3"}]
            preview  (conversation/conversation-preview-text messages)]
        (is (= 3 (count preview)))
        (is (not (str/includes? (str preview) "more messages")))))))

(deftest conversation-modal-scrollable-test
  ;; Test that ConversationModal renders with scrollable container
  (testing "ConversationModal has scrollable container"
    (let [messages [{:role :user :content "Message 1"}
                    {:role :assistant :content "Message 2"}]
          modal-element (conversation/ConversationModal {:messages messages})
          modal-props (.-props modal-element)
          class-name (.-className modal-props)]
      (testing "has max-height constraint"
        (is (str/includes? class-name "max-h-[600px]")
            "Modal should have max-height to enable scrolling"))
      (testing "has vertical overflow scroll"
        (is (str/includes? class-name "overflow-y-auto")
            "Modal should have overflow-y-auto for vertical scrolling")))))
