(ns lemmalearnerclj.learner-test
  (:require
   [clojure.pprint :refer :all]
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.learner :refer :all]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatabase :refer :all]
   [lemmalearnerclj.textdatastructures :refer :all]
   [clojure.pprint :as pprint]
   [clojure.set :as set])
  (:import
   [lemmalearnerclj.textdatastructures Sentence Conjugation Lemma]))

(def test-sentence1 (parser/parse-raw-sentence "Dette er det."))
(def test-sentence2 (parser/parse-raw-sentence "Dette også det."))

(def test-config
  {:language "danish"
   :learning-config {:drop-off-factor 0.5
                     :max-lemma-times-learned 3
                     :max-lemmas-to-learn 1000}})

(def test-text-db
  (lemmalearnerclj.textdatabase/directory->text-db test-config "test/lemmalearnerclj/test_files/test_text_folder/"))

(deftest test-sentence-to-lemmas
  (testing "Sentences gives correct lemmas"
    (sentence->lemmas test-text-db test-sentence1)))

(defn raw-text->new-learn-info [raw-text]
  (->> raw-text
       (parser/parse-raw-text "test")
       list
       (lemmalearnerclj.textdatabase/texts->text-db test-config)
       (text-db->new-learn-info test-config)))

(def large-learn-info
  (directory->new-learn-info test-config "test/lemmalearnerclj/test_files/larger_texts/"))

(def true-large-learn-info
  (directory->new-learn-info (assoc test-config :language "english") "test/lemmalearnerclj/test_files/larger_texts/"))

(def learned-true-large-text
  (learn-all-lemmas true-large-learn-info))

(def learned-large-text
  (learn-all-lemmas large-learn-info))


(deftest test-correct-initialized-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learn-info "Lære. Kage. Kage test.")
          learnable-sentences (->> simple-information :learn-db :sentences-by-score seq (map #(identity [(:raw (first %)) (second %)])))]
      (is (= (seq [["Kage." -2.0] ["Lære." -1.0]])
             learnable-sentences)))))

(deftest test-learn-sentence-updates-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learn-info "Sætning. Sætning et. Sætning to.")
          start-learnable-sentences (->> simple-information :learn-db :sentences-by-score seq (map first) (map :raw) set)
          learned-sentence (learn-top-sentence simple-information)
          learnable-sentences (->> learned-sentence :learn-db :sentences-by-score seq (map first) (map :raw) set)]
      (is (= #{"Sætning."}
             start-learnable-sentences))
      (is (= #{"Sætning et." "Sætning to."}
             learnable-sentences)))))

(deftest test-learn-sentence-updates-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learn-info "Test. Dette er en test sætning.")
          learned-sentence (learn-top-sentence simple-information)
          lemmas-by-score-before (->> simple-information :learn-db :lemmas-by-score)
          lemmas-by-score-after (->> learned-sentence :learn-db :lemmas-by-score)]
      (is (= (dissoc lemmas-by-score-before (Lemma. "test"))
             lemmas-by-score-after)))))

(deftest test-learn-word-updates-sentences-by-score
  (testing ""
    (let [simple-information (raw-text->new-learn-info "Kan. Kunne. Kan læres.")
          learned-sentence (learn-top-sentence simple-information)
          _ (learn-top-sentence learned-sentence)
          learnable-sentences (->> learned-sentence :learn-db :sentences-by-score seq (map first) (map :raw) set)]
      (is (= #{"Kan læres."}
             learnable-sentences)))))

(deftest test-text-db-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [word->frequency (text-db->lemma->frequency test-text-db)]
        (is (= (helper/record->map word->frequency)
             {{:raw "denne"} -2, {:raw "sætning"} -2, {:raw "et"} -2, {:raw "race"} -2, {:raw "endnu"} -1})))))

(deftest test-sentences-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [lemmas-by-frequencies (->> test-text-db
                                     :sentences
                                     (sentences->lemmas-by-frequency test-text-db))]
      (is (= (helper/record->map lemmas-by-frequencies)
             {{:raw "denne"} -2, {:raw "sætning"} -2, {:raw "en"} -2, {:raw "race"} -2, {:raw "endnu"} -1}))
      )))

(deftest test-text-db-to-words-by-frequencies
  (testing "Words do not have the correct frequencies"
    (let [word->frequency (text-db->lemma->frequency test-text-db)]
        (is (= (update-keys word->frequency :raw)
               {"race" -2, "denne" -2, "en" -2, "sætning" -2, "endnu" -1})))))

(deftest test-learned-sentences-correct
  (testing ""
    (let [mock-learning-information (raw-text->new-learn-info "Dette er det. Dette er også det.")
          learned-sentences (learn-sentences mock-learning-information [test-sentence1 test-sentence2] [nil nil])
          learned-sentence1 (learn-sentence mock-learning-information test-sentence1 nil)
          learned-sentence2 (learn-sentence learned-sentence1 test-sentence2 nil)]
          ;word-to-times-learned (update-keys (->> learned-sentences :learn-prog :word-to-times-learned) :raw) ]
      (is (= learned-sentence2
             learned-sentences)))))

(deftest test-learned-sentences
  (testing ""
    (let [mock-learning-information (raw-text->new-learn-info "Dette er det. Dette er også det.")
          learned-sentences (learn-sentences mock-learning-information [test-sentence1 test-sentence2] [-1.0 -2.0])
          conj->#learned (update-keys (->> learned-sentences :learn-prog :conj->#learned) :raw) ]
      (is (= {"dette" 2, "det" 2, "er" 1, "også" 1} conj->#learned))
      (is (= 4 (count-lemmas-learned learned-sentences)))
      (is (= (->> large-learn-info :learn-db :words-by-score count )
             (->> learned-sentences :learn-db :words-by-score count))))))

(deftest test-learnable
  (testing ""
    (let [learned-no-sentence (raw-text->new-learn-info "Dette er det. Dette er også det.")
          learned-first-sentence  (learn-sentence learned-no-sentence test-sentence1 -100.0)
          learned-second-sentence (learn-sentence learned-first-sentence test-sentence2 nil)
          second-sentence-learnable (learnable? learned-first-sentence test-sentence2)
          ]
      (is (not (learnable? learned-no-sentence test-sentence1)))
      (is (not (learnable? learned-no-sentence test-sentence2)))
      (is (not (learnable? learned-first-sentence test-sentence1)))
      (is second-sentence-learnable true)
      (is (not (learnable? learned-second-sentence test-sentence2))))))

(deftest test-get-a-lemma
  (testing "Did not return a lemma word"
    (let [unlearned-lemma (get-a-unlearned-lemma large-learn-info)]
      (is (not (nil? unlearned-lemma))))))


(deftest test-all-lemmas-learned-after-finished-learning
  (testing ""
    (let [all-lemmas (->> large-learn-info :text-db :lemmas set)
          learned-words (->> learned-large-text
                             :learn-prog
                             :learning-order
                             (map :lemma)
                             set)]
      (is (= learned-words all-lemmas)))))

(deftest test-all-words-learned-greedily
  (testing ""
    (is (= 1 1))))

;;
