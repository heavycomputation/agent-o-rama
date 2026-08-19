(ns com.rpl.agent.basic.structured-openai-agent
  "Demonstrates native OpenAI structured output with a JSON output schema.

  Features demonstrated:
  - JSON schema definition with schema/strict-object and field types
  - Structured output via the :output-schema request key
  - OpenAI chat model integration with structured outputs
  - Single-node agent returning structured data via :parsed"
  (:require
   [com.rpl.agent-o-rama :as aor]
   [com.rpl.agent-o-rama.model :as model]
   [com.rpl.agent-o-rama.model.openai :as openai]
   [com.rpl.agent-o-rama.schema :as schema]
   [com.rpl.rama :as rama]
   [com.rpl.rama.test :as rtest]))

;;; JSON schema for structured response (strict-object marks every property
;;; required and disallows additional properties, as strict mode demands)
(def QuestionAnalysis
  (schema/strict-object
   {:description "Analysis of a user question with structured breakdown"}
   {"question_type" (schema/enum
                     "Type of question being asked"
                     ["factual" "analytical" "creative" "technical" "personal"])
    "complexity"    (schema/enum
                     "Complexity level of the question"
                     ["simple" "moderate" "complex"])
    "main_topics"   (schema/array
                     "Key topics covered in the question"
                     (schema/string "A main topic or concept"))
    "answer"        (schema/string "Direct answer to the user's question")
    "confidence"    (schema/enum
                     "Confidence level in the response"
                     ["high" "medium" "low"])}))

;;; Agent module demonstrating structured OpenAI output
(aor/defagentmodule StructuredOpenAIAgentModule
  [topology]

  ;; Declare OpenAI chat model as an auto-traced agent object
  (openai/declare-model
   topology
   "openai-model"
   {:api-key-env "OPENAI_API_KEY"
    :model       "gpt-5-mini"
    :reasoning   {:effort :low}})

  (->
    (aor/new-agent topology "StructuredOpenAIAgent")

    ;; Single node that analyzes user question and returns structured response
    (aor/node
     "analyze-question"
     nil
     (fn [agent-node ^String user-question]
       (let
         [model      (aor/get-agent-object agent-node "openai-model")
          system-msg
          (model/system
           "You are an intelligent question analyzer. Analyze the user's question and provide a structured response that categorizes the question type, assesses its complexity, identifies main topics, provides a direct answer, and indicates your confidence level.")
          user-msg   (model/user user-question)
          ;; Configure structured JSON output; the response's :parsed key
          ;; contains the decoded structure
          response   (model/chat
                      model
                      {:messages      [system-msg user-msg]
                       :output-schema {:name   "QuestionAnalysis"
                                       :schema QuestionAnalysis}})]

         ;; Return the already-parsed structured response
         (aor/result! agent-node (:parsed response)))))))

(defn -main
  "Run the structured OpenAI agent example"
  [& _args]
  (if (System/getenv "OPENAI_API_KEY")
    (with-open [ipc (rtest/create-ipc)]
      (rtest/launch-module! ipc StructuredOpenAIAgentModule {:tasks 1 :threads 1})

      (let [module-name (rama/get-module-name StructuredOpenAIAgentModule)
            manager     (aor/agent-manager ipc module-name)
            agent       (aor/agent-client manager "StructuredOpenAIAgent")]

        (println "Structured OpenAI Agent Example:")
        (println "Analyzing questions with structured output...\n")

        ;; Test with different types of questions
        (doseq [question ["What is artificial intelligence?"
                          "How can I improve my programming skills?"
                          "Write a creative story about a robot"]]
          (println "Question:" question)
          (let [result (aor/agent-invoke agent question)]
            (println "Analysis:")
            (println "  Type:" (get result "question_type"))
            (println "  Complexity:" (get result "complexity"))
            (println "  Topics:" (get result "main_topics"))
            (println "  Answer:" (get result "answer"))
            (println "  Confidence:" (get result "confidence"))
            (println)))

        (println "Notice how:")
        (println "- JSON schema defines the exact structure expected")
        (println "- :output-schema ensures structured output")
        (println "- Different question types are automatically categorized")
        (println "- Response includes metadata about the analysis")))

    (do
      (println "Structured OpenAI Agent Example:")
      (println "OPENAI_API_KEY environment variable not set.")
      (println "Please set your OpenAI API key to run this example:")
      (println "  export OPENAI_API_KEY=your-api-key-here"))))
