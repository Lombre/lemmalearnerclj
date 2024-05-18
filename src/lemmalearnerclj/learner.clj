(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.math :as math]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures Sentence Conjugation Lemma]))

(require '[clojure.data.priority-map :refer [priority-map]])
(require '[clojure.core.reducers :as reducers])

(defrecord Score-point [lemma sentence score])

(defrecord Learning-progress [word-to-times-learned learning-order])

(defrecord Learning-database [sentences-by-score lemmas-by-score lemma->frequency])

(defrecord Learning-information [learning-progress learning-database text-database])

(defn conjugation->lemma [text-database conjugation]
  (get (:conjugation->lemma text-database) conjugation ))

(defn sentence->lemmas [text-database sentence]
  (->> sentence
       :words
       (map #(conjugation->lemma text-database %) )
       (filter some?)))

(defn lemma->sentences [text-database lemma]
  (->> lemma
       (get (:lemma->conjugations text-database) )
       (mapcat #(get (:conjugation->sentences text-database) %))))

(defn word->times-learned [learning-progress word]
  (get (:word-to-times-learned learning-progress) word 0))

(defn lemma->times-learned [learning-progress text-database lemma]
  (->> lemma
       (get (:lemma->conjugations text-database))
       (map #(word->times-learned learning-progress %))
       (reduce +)))

(defn sentence->unlearned-lemmas [learning-progress text-database sentence]
  (->> (sentence->lemmas text-database sentence)
       (filter #(= 0 (lemma->times-learned learning-progress text-database %)))
       set))

(defn merge-frequencies [frequencies]
  (reducers/fold (partial merge-with +) frequencies))

(defn sentences->lemmas-by-frequency [text-database sentences]
  (->> sentences
       (map #(sentence->lemmas text-database %))
       (map frequencies)
       (merge-frequencies)
       (clojure.core/reduce-kv #(assoc %1 %2 (- %3)) (priority-map)))) ; Transfer to priority map

(defn text-database->lemma->frequency [text-database]
  (sentences->lemmas-by-frequency text-database (:sentences text-database)))

(defn learnable? [learning-progress text-database sentence]
  (= (count (sentence->unlearned-lemmas learning-progress text-database sentence))
     1))

(defn score-by-lemma-frequency [lemma lemma->frequency text-database learning-progress]
  (* (get lemma->frequency lemma 0)
     (Math/pow 0.1 (lemma->times-learned learning-progress text-database lemma))))

(defn score-sentence [lemma->frequency learning-progress text-database sentence]
  (->> (sentence->lemmas text-database sentence)
       (map #(score-by-lemma-frequency % lemma->frequency text-database learning-progress))
       (reducers/reduce +)
       -))

(defn sentences->sentences-by-score [learning-progress text-database lemma->frequency sentences]
  (->> sentences
       (filter #(learnable? learning-progress text-database %))
       (reducers/reduce (fn [xs x] (assoc xs x (score-sentence lemma->frequency learning-progress text-database x)))
                        (priority-map))))

(defn text-database->learning-database [text-database]
  (let [sentences-by-score (sentences->sentences-by-score (->Learning-progress {} []) text-database (text-database->lemma->frequency text-database) (:sentences text-database))
        lemmas-by-score (sentences->lemmas-by-frequency text-database (:sentences text-database))
        lemmas-by-frequency (text-database->lemma->frequency text-database)]
    (->Learning-database sentences-by-score lemmas-by-score lemmas-by-frequency)))

(defn update-sentences-by-scores [learning-progress learning-database text-database learned-lemma]
  (let [sentences-by-score (:sentences-by-score learning-database)]
    (->> (lemma->sentences text-database learned-lemma)
         (filter #(learnable? learning-progress text-database %))
         (reducers/reduce (fn [xs x] (assoc xs x (score-sentence (:word->frequency learning-database) learning-progress text-database x))) sentences-by-score))))

(defn update-learning-database-with-learned-words [learning-progress learning-database text-database lemmas]
;  (println words)
  (if (< 0 (count lemmas))
    (let [lemma (first lemmas)
          updated-sentences-by-scores (update-sentences-by-scores learning-progress learning-database text-database lemma)
          updated-lemmas-by-score (dissoc (:lemmas-by-score learning-database) lemma)
          updated-database (->Learning-database updated-sentences-by-scores updated-lemmas-by-score (:lemma->frequency learning-database))]
      (recur learning-progress updated-database text-database (rest lemmas)))
    learning-database))

(defn update-times-learned [word-to-times-learned words]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) word-to-times-learned words))

(defn update-learning-order [learning-progress sentence unlearned-words score]
  ;; Append all the learned (word, sentence, score) pair
  (reducers/reduce #(conj %1 (->Score-point %2 sentence score)) (:learning-order learning-progress) unlearned-words ))

(defn learn-sentence [learning-information sentence score]
;  (println sentence)
  (let [learning-progress (:learning-progress learning-information)
        learning-database (:learning-database learning-information)
        unlearned-lemmas (sentence->unlearned-lemmas learning-progress (:text-database learning-information) sentence)
        updated-learning-order (update-learning-order learning-progress sentence unlearned-lemmas score)
        updated-times-learned (update-times-learned (:word-to-times-learned learning-progress) (:words sentence))
        updated-progress (->Learning-progress updated-times-learned updated-learning-order)
        updated-learning-database (update-learning-database-with-learned-words updated-progress learning-database (:text-database learning-information) unlearned-lemmas)]
    (->Learning-information updated-progress updated-learning-database (:text-database learning-information))))

(defn learn-sentences [learning-information sentences scores]
  (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                   learning-information (map vector sentences scores)))

(defn get-a-unlearned-lemma [learning-information]
;  (println (take 10 (->> learning-information :learning-database :words-by-score)))
  (let [[lemma _] (->> learning-information :learning-database :lemmas-by-score first)]
    lemma))

(defn pop-top-sentence [learning-information]
  (if (not (empty? (->> learning-information :learning-database :sentences-by-score)))
   (let [[top-sentence top-sentence-score] (->> learning-information :learning-database :sentences-by-score first)
         remaining-sentences-by-score (->> learning-information :learning-database :sentences-by-score pop)]
     [top-sentence top-sentence-score remaining-sentences-by-score])
   (let [unlearned-lemma (get-a-unlearned-lemma learning-information)]
     (if (nil? unlearned-lemma) (throw (Exception. "Error: Trying to learn an unlearned lemma, when there are non left.")) nil)
     [(Sentence. (str "NoSentence: " (:raw unlearned-lemma)) [] [(Conjugation. (:raw unlearned-lemma))]) nil
      (->> learning-information :learning-database :sentences-by-score)])))


(defn count-lemmas-learned [learning-information]
  (->> learning-information :learning-progress :learning-order count))

(defn finished-learning? [learning-information]
  (if (zero? (mod (count-lemmas-learned learning-information) 1)) (println (count-lemmas-learned learning-information))
                                                                   nil)
  (>= (count-lemmas-learned learning-information) (->> learning-information :text-database :lemmas count)))

(defn learn-top-sentence [learning-information]
  (if (finished-learning? learning-information) nil
   (let [[top-sentence top-sentence-score remaining-sentences-by-score] (pop-top-sentence learning-information)
         updated-database (assoc (:learning-database learning-information) :sentences-by-score remaining-sentences-by-score)
         updated-learning-information (assoc learning-information :learning-database updated-database)]
     (Thread/sleep 1)
     (cond                    ; Sometimes all lemma have been learned in the top sentence, so we just skip it.
       (learnable? (:learning-progress learning-information) (:text-database learning-information) top-sentence)
       (learn-sentence updated-learning-information top-sentence top-sentence-score)
       :else
       (recur updated-learning-information)))))

(defn merge-frequencies [text-lemma-frequencies]
  (reducers/fold (partial merge-with +) text-lemma-frequencies))

(defn learn-all-lemmas [learning-information]
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

(defn score-point-to-str [{:keys [lemma sentence score]}]
  (str (:raw lemma) " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))
