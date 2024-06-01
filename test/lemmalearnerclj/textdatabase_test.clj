(ns lemmalearnerclj.textdatabase-test
  (:require [lemmalearnerclj.textdatastructures]
            [lemmalearnerclj.parser :as parser]
            [lemmalearnerclj.textdatabase :refer :all]
            [clojure.test :refer :all]
            [clojure.set])
  (:import [lemmalearnerclj.textdatastructures Text Paragraph Sentence Conjugation]))

(def simple-text
  (parser/parse-raw-text "test" "This is a line.\n And another line."))

(def simple-sentences
  (texts->sentences [simple-text]))

(deftest test-text-to-sentences
  (testing
      (let [sentences simple-sentences
            expected-sentences ["This is a line.", "And another line."]]
        (is (= (map :raw sentences)
               expected-sentences)))))


(let [short-sentence (Sentence. "This sentence is short and learnable." [] [])
      long-sentence (Sentence. "This is a long test sentence, that should be filtered out, because of its immense length which makes it basicly unlearnable." [] [])]
  (filter-sentences-for-learning [long-sentence]))

(deftest test-sentences-to-words
  (testing
      (let [words (sentences->words simple-sentences)
            expected-words #{"this" "is" "a" "line" "and" "another"}]
        (is (= (set (map :raw words))
               expected-words)))))

(deftest test-words-to-sentences
  (testing
      (let [word->sentences (sentences->word->sentences simple-sentences)
            actual-mapping (update-vals (update-keys word->sentences :raw) #(set (map :raw %)))]
        (is (= actual-mapping
               {"this" #{"This is a line."}
                "is" #{"This is a line."}
                "a" #{"This is a line."}
                "line" #{"This is a line." "And another line."}
                "and" #{"And another line."}
                "another" #{"And another line."}})))))

(deftest test-create-textdatabase
  (testing
      (let [words (sentences->words simple-sentences)
            expected-words #{"this" "is" "a" "line" "and" "another"}]
        (is (= (set (map :raw words))
               expected-words)))))
