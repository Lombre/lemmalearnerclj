(ns lemmalearnerclj.learner-test
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.pprint :refer :all]
   [clojure.test :refer :all]
   [lemmalearnerclj.learner :refer :all]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatabase]
   [lemmalearnerclj.textdatastructures :refer :all]))


(def test-sentence1 (parser/parse-raw-sentence "This is it."))
(def test-sentence2 (parser/parse-raw-sentence "This also it."))

(def test-text-database
  (lemmalearnerclj.textdatabase/directory->text-database "test/lemmalearnerclj/test_files/test_text_folder/"))

(defn raw-text->new-learning-information [raw-text]
  (->> raw-text
       (parser/parse-raw-text "test")
       list
       lemmalearnerclj.textdatabase/texts->text-database
       text-database->new-learning-information))

(def blank-learning-information
  (text-database->new-learning-information test-text-database))


(def large-learning-information
  (directory->new-learning-information "test/lemmalearnerclj/test_files/larger_texts/"))

(def learned-large-text
  (learn-all-words large-learning-information))

(deftest test-correct-initialized-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learning-information "Learnable.")
          learnable-sentences (->> simple-information :learning-database :sentences-by-score seq (map first) (map :raw) set)]
        (is (= #{"Learnable."}
               learnable-sentences)))))

(deftest test-learn-sentence-updates-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learning-information "Learnable. Learnable sentence. Learnable word.")
          learned-sentence (learn-top-sentence simple-information)
          learnable-sentences (->> learned-sentence :learning-database :sentences-by-score seq (map first) (map :raw) set)]
        (is (= #{"Learnable sentence." "Learnable word."}
               learnable-sentences)))))

(deftest test-learn-word-updates-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learning-information "Not learnable.")
          learned-sentence (learn-top-sentence simple-information)
          learnable-sentences (->> learned-sentence :learning-database :sentences-by-score seq (map first) (map :raw) set)]
        (is (= #{"Not learnable."}
               learnable-sentences)))))

(deftest test-text-database-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [word->frequency (text-database->word->frequency test-text-database)]
        (is (= (update-keys word->frequency :raw)
               {"this" 2, "another" 1, "line" 2, "is" 2, "a" 1})))))

(deftest test-sentences-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [words-by-frequencies (->> test-text-database
                                    :sentences
                                    sentences->words-by-frequency)]
        (is (= (update-keys words-by-frequencies :raw)
               {"this" -2, "line" -2, "is" -2, "a" -1, "another" -1})))))

(deftest test-text-database-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [word->frequency (text-database->word->frequency test-text-database)]
        (is (= (update-keys word->frequency :raw)
               {"this" 2, "another" 1, "line" 2, "is" 2, "a" 1})))))

(deftest test-learned-sentences-correct
  (testing ""
    (let [learned-sentences (learn-sentences large-learning-information [test-sentence1 test-sentence2] [nil nil])
          learned-sentence1 (learn-sentence large-learning-information test-sentence1 nil)
          learned-sentence2 (learn-sentence learned-sentence1 test-sentence2 nil)]
          ;word-to-times-learned (update-keys (->> learned-sentences :learning-progress :word-to-times-learned) :raw) ]
      (is (= learned-sentence2
             learned-sentences)))))

(deftest test-learned-sentences
  (testing ""
    (let [learned-sentences (learn-sentences large-learning-information [test-sentence1 test-sentence2] [nil nil])
          word-to-times-learned (update-keys (->> learned-sentences :learning-progress :word-to-times-learned) :raw) ]
      (is (and (= {"this" 2, "it" 2, "is" 1, "also" 1} word-to-times-learned)
               (= 4 (count-words-learned learned-sentences))
               (= (- (->> large-learning-information :learning-database :words-by-score count ) 4)
                  (->> learned-sentences :learning-database :words-by-score count)))))))

(deftest test-learnable
  (testing ""
    (let [learned-no-sentence large-learning-information
          learned-first-sentence  (learn-sentence learned-no-sentence test-sentence1 nil)
          learned-second-sentence (learn-sentence learned-first-sentence test-sentence2 nil)]
      (is (and
        (not (learnable? (:learning-progress learned-no-sentence) test-sentence1))
        (not (learnable? (:learning-progress learned-no-sentence) test-sentence2))
        (not (learnable? (:learning-progress learned-first-sentence) test-sentence1))
        (learnable? (:learning-progress learned-first-sentence) test-sentence2)
        (not (learnable? (:learning-progress learned-second-sentence) test-sentence2)))))))

(deftest test-get-a-unlearned-word
  (testing "Did not return a unlearned word"
    (let [unlearned-word (get-a-unlearned-word blank-learning-information)]
      (is (not (nil? unlearned-word))))))


(deftest test-all-words-learned-after-finished-learning
  (testing ""
    (let [all-words (->> large-learning-information :text-database :words set)
          learned-words (->> learned-large-text
                             :learning-progress
                             :learning-order
                             (map first)
                             set)]
      (is (= learned-words all-words)))))

(deftest test-all-words-learned-greedily
  (testing ""
    (is (= 1 1))))

;; (->> learned-large-text :learning-progress :learning-order (map score-point-to-str)
;; ;;      (#(map vector (range 1 (count %)) (repeat (count %) ") ") %))
;; ;;      (map #(reducers/reduce str %))
;; ;;      (take 100)
;; ;;      (run! pprint))
