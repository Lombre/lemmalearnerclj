(ns lemmalearnerclj.textdatabase-test
  (:require
   [clojure.pprint :as pprint]
   [clojure.set]
   [clojure.test :refer :all]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatabase :refer :all]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures Sentence Lemma Conjugation]))

(def parsing-config {:punctuation #{\. \! \?}
                    :quote-pairs {\" \"
                                  \“ \”
                                  \' \'
                                  \( \)
                                  \[ \]
                                  \¿ \?
                                  \« \»
                                  \¡ \!}
                    :other-punctuation #{\. \; \:}})

(def simple-text
  (parser/parse-raw-text parsing-config "test" "This is a line.\n And another line."))

(def simple-sentences
  (texts->sentences [simple-text]))

(deftest test-text-to-sentences
  (testing
      (let [sentences simple-sentences
            expected-sentences ["This is a line.", "And another line."]]
        (is (= (map :raw sentences)
               expected-sentences)))))

(deftest test-filter-sentences-for-learning-filters-long-sentences
  (testing
      (let [short-sentence (Sentence. "This sentence is short and learnable." [] [])
            long-sentence (Sentence. "This is a long test sentence, that should be filtered out, because of its immense length which makes it basicly unlearnable." [] [])]
        (is (= (list short-sentence)
             (filter-sentences-for-learning [short-sentence long-sentence]))))))

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


(def test-textdb (->Textdatabase nil nil nil nil nil
                                {(Lemma. "cake") #{(Conjugation. "cake") (Conjugation. "cakes")}}
                                {(Conjugation. "cake") (Lemma. "cake") (Conjugation. "cakes") (Lemma. "cake")}))


;; (defn update-lemmatization [text-db conjugation new-lemma]
;;   (let [old-lemmatization (get (->> text-db :conjugation->lemma) conjugation)
;;         updated-text-db (->> text-db
;;                              (#(assoc-in % [:conjugation->lemma conjugation] new-lemma))
;;                              ;; (#(assoc-in % [:lemma->conjugations new-lemma]))
;;                              )
;;         ]
;;     updated-text-db))

;; (pprint/pprint (update-lemmatization test-textdb (Conjugation. "cake") (Lemma. "cakes")))
