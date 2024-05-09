(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.math :as math]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures Sentence]))

(require '[clojure.data.priority-map :refer [priority-map]])
(require '[clojure.core.reducers :as reducers])

(defrecord Learning-progress [word-to-times-learned learning-order])

(defrecord Learning-database [sentences-by-score words-by-score word->frequency])

(defrecord Learning-information [learning-progress learning-database text-database])

(defn word->times-learned [learning-progress word]
  (get (:word-to-times-learned learning-progress) word 0))

(defn sentence->unlearned-words [learning-progress sentence]
  (filter #(= 0 (word->times-learned learning-progress %)) (:words sentence)))

(defn merge-frequencies [text-word-frequencies]
  (reducers/fold (partial merge-with +) text-word-frequencies))

(defn text-database->word->frequency [text-database]
  (->> text-database
       (:sentences)
       (map :words)
       (map frequencies)
       (merge-frequencies)))

(defn sentences->words-by-frequency [sentences]
  (->> sentences
       (map :words)
       (map frequencies)
       (merge-frequencies)
       (clojure.core/reduce-kv #(assoc %1 %2 (- %3)) (priority-map)))) ; Transfer to priority map

(defn learnable? [learning-progress sentence]
  (= (count (sentence->unlearned-words learning-progress sentence)) 1))

(defn score-by-word-frequency [word word->frequency learning-progress]
  (* (get word->frequency word)
     (Math/pow 0.1 (word->times-learned learning-progress word))))

(defn score-sentence [word->frequency learning-progress sentence]
  (->> (:words sentence)
       (map #(score-by-word-frequency % word->frequency learning-progress))
       (reducers/reduce +)
       -))

(defn sentences->sentences-by-score [learning-progress word->frequency sentences]
  (->> sentences
       (filter #(learnable? learning-progress %))
       (reducers/reduce (fn [xs x] (assoc xs x (score-sentence word->frequency learning-progress x))) (priority-map))))

(defn text-database->learning-database [text-database]
  (let [sentences-by-score (sentences->sentences-by-score (->Learning-progress {} []) (text-database->word->frequency text-database) (:sentences text-database))
        words-by-score (sentences->words-by-frequency (:sentences text-database))
        words-by-frequency (text-database->word->frequency text-database)]
    (->Learning-database sentences-by-score words-by-score words-by-frequency)))

(defn update-sentences-by-scores [learning-progress learning-database text-database learned-word]
  (let [sentences-by-score (:sentences-by-score learning-database)]
    (->> (get (:word->sentences text-database) learned-word)
         (filter #(learnable? learning-progress %))
         (reducers/reduce (fn [xs x] (assoc xs x (score-sentence (:word->frequency learning-database) learning-progress x))) sentences-by-score))))

(defn update-learning-database-with-learned-words [learning-progress learning-database text-database words]
;  (println words)
  (if (< 0 (count words))
    (let [word (first words)
          updated-sentences-by-scores (update-sentences-by-scores learning-progress learning-database text-database word)
          updated-word-by-score (dissoc (:words-by-score learning-database) word)
          updated-database (->Learning-database updated-sentences-by-scores updated-word-by-score (:word->frequency learning-database))]
      (recur learning-progress updated-database text-database (rest words)))
    learning-database))

(defn update-times-learned [word-to-times-learned words]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) word-to-times-learned words))

(defn update-learning-order [learning-progress sentence unlearned-words score]
  ;; Append all the learned (word, sentence, score) pair
  (reducers/reduce #(conj %1 [%2 sentence score]) (:learning-order learning-progress) unlearned-words ))

(defn learn-sentence [learning-information sentence score]
;  (println sentence)
  (let [learning-progress (:learning-progress learning-information)
        learning-database (:learning-database learning-information)
        unlearned-words (sentence->unlearned-words learning-progress sentence)
        updated-learning-order (update-learning-order learning-progress sentence unlearned-words score)
        updated-times-learned (update-times-learned (:word-to-times-learned learning-progress) (:words sentence))
        updated-progress (->Learning-progress updated-times-learned updated-learning-order)
        updated-learning-database (update-learning-database-with-learned-words updated-progress learning-database (:text-database learning-information) unlearned-words)]
    (->Learning-information updated-progress updated-learning-database (:text-database learning-information))))

(defn learn-sentences [learning-information sentences scores]
  (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                   learning-information (map vector sentences scores)))

(defn get-a-unlearned-word [learning-information]
;  (println (take 10 (->> learning-information :learning-database :words-by-score)))
  (let [[word _] (->> learning-information :learning-database :words-by-score first)]
    word))

(defn pop-top-sentence [learning-information]
  (if (not (empty? (->> learning-information :learning-database :sentences-by-score)))
   (let [[top-sentence top-sentence-score] (->> learning-information :learning-database :sentences-by-score first)
         remaining-sentences-by-score (->> learning-information :learning-database :sentences-by-score pop)]
     [top-sentence top-sentence-score remaining-sentences-by-score])
   (let [unlearned-word (get-a-unlearned-word learning-information)]
     [(Sentence. (str "NoSentence: " (:raw unlearned-word)) [] [unlearned-word]) nil (->> learning-information :learning-database :sentences-by-score)])))


(defn count-words-learned [learning-information]
  (->> learning-information :learning-progress :learning-order count))

(defn finished-learning? [learning-information]
  (if (zero? (mod (count-words-learned learning-information) 100)) (println (count-words-learned learning-information))
                                                                   nil)
  (= (count-words-learned learning-information) (->> learning-information :text-database :words count)))

(defn learn-top-sentence [learning-information]
  (if (finished-learning? learning-information) nil
   (let [[top-sentence top-sentence-score remaining-sentences-by-score] (pop-top-sentence learning-information)
         updated-database (assoc (:learning-database learning-information) :sentences-by-score remaining-sentences-by-score)
         updated-learning-information (assoc learning-information :learning-database updated-database)]
      ; Sometimes all words have been learned in the top sentence, so we just skip it.
     (cond
       (learnable? (:learning-progress learning-information) top-sentence)
           (learn-sentence updated-learning-information top-sentence top-sentence-score)
       :else
          (recur updated-learning-information)))))

(defn merge-frequencies [text-word-frequencies]
  (reducers/fold (partial merge-with +) text-word-frequencies))

(defn learn-top-n-sentences [n learning-information]
  (cond (<= n 0)
        learning-information
        :else
        (learn-top-n-sentences (dec n) (learn-top-sentence learning-information))))

(defn learn-all-words [learning-information]
  (let [updated-learning-information (learn-top-sentence learning-information)]
    (if (not (nil? updated-learning-information))
      (recur updated-learning-information)
      learning-information)))

(defn text-database->new-learning-information [text-database]
  (let [learning-progress (->Learning-progress {} [])
        learning-database (text-database->learning-database text-database)
        learning-information (->Learning-information learning-progress learning-database text-database)]
    learning-information))

(defn directory->new-learning-information [directory]
  (text-database->new-learning-information (lemmalearnerclj.textdatabase/directory->text-database directory)))


(defn score-point-to-str [[word sentence score]]
  (str (:raw word) " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))
