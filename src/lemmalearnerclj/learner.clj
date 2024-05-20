(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.math :as math]
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures Conjugation Sentence]))

(require '[clojure.data.priority-map :refer [priority-map]])
(require '[clojure.core.reducers :as reducers])

(defrecord Score-point [lemma sentence score])

(defrecord Learning-progress [conjugation-to-times-learned learning-order])

(defrecord Learning-database [sentences-by-score lemmas-by-score lemma->frequency])

(defrecord Learning-information [learning-progress learning-database text-database config])

(defn conjugation->lemma [text-database conjugation]
  (get (:conjugation->lemma text-database) conjugation ))

(defn sentence->lemmas [text-database sentence]
  (->> sentence
       :words
       (map #(conjugation->lemma text-database %))
       (filter some?)))

(defn lemma->sentences [text-database lemma]
  (->> lemma
       (get (:lemma->conjugations text-database) )
       (mapcat #(get (:conjugation->sentences text-database) %))))

(defn conjugation->times-learned [learning-progress word]
  (get (:conjugation-to-times-learned learning-progress) word 0))

(defn lemma->times-learned [learning-progress text-database lemma]
  (->> lemma
       (get (:lemma->conjugations text-database))
       (map #(conjugation->times-learned learning-progress %))
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

(defn score-by-lemma-frequency [lemma->frequency text-database learning-progress lemma]
  (* (get lemma->frequency lemma 0)
     (Math/pow 0.1 (lemma->times-learned learning-progress text-database lemma))))

(defn score-sentence [lemma->frequency learning-progress text-database sentence]
  (->> (sentence->lemmas text-database sentence)
       (map (partial score-by-lemma-frequency lemma->frequency text-database learning-progress))
       (reducers/reduce +)))

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
         (reducers/reduce (fn [xs x] (if (learnable? learning-progress text-database x)
                                      (assoc xs x (score-sentence (:lemma->frequency learning-database) learning-progress text-database x))
                                      (dissoc xs x))) sentences-by-score))))

(defn update-learning-database-with-learned-lemmas [learning-progress learning-database text-database [lemma & rem-lemmas]]
  (if (nil? lemma) learning-database
    (let [updated-sentences-by-scores (update-sentences-by-scores learning-progress learning-database text-database lemma)
          updated-lemmas-by-score (dissoc (:lemmas-by-score learning-database) lemma)
          updated-database (->Learning-database updated-sentences-by-scores updated-lemmas-by-score (:lemma->frequency learning-database))]
      (recur learning-progress updated-database text-database rem-lemmas))))

(defn update-times-learned [conjugation-to-times-learned conjugations]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) conjugation-to-times-learned conjugations))

(defn update-learning-order [learning-progress sentence unlearned-lemmas score]
  ;; Append all the learned (word, sentence, score) pair
  (reducers/reduce #(conj %1 (->Score-point %2 sentence score)) (:learning-order learning-progress) unlearned-lemmas ))

(defn count-lemmas-learned [learning-information]
  (->> learning-information :learning-progress :learning-order count))

(defn- print-current-learning-status [learning-information sentence score]
  (let [total-lemma-count (->> learning-information :text-database :lemmas count)
        current-lemma-count (+ 1 (count-lemmas-learned learning-information))
        unlearned-lemmas (sentence->unlearned-lemmas (:learning-progress learning-information)
                                                     (:text-database learning-information) sentence)
        message (str current-lemma-count " of " total-lemma-count ", " (set (map :raw unlearned-lemmas)) " "
                     (format "%.2f" (if (nil? score) 0.0 score)) " -> " (:raw sentence))]
    (helper/print-if (or (zero? (mod current-lemma-count 100))
                         (>= 100  current-lemma-count)
                         (= current-lemma-count total-lemma-count))
                     message)))

(defn learn-sentence [learning-information sentence score]
  (let [learning-progress (:learning-progress learning-information)
        learning-database (:learning-database learning-information)
        unlearned-lemmas (sentence->unlearned-lemmas learning-progress (:text-database learning-information) sentence)
        updated-learning-order (update-learning-order learning-progress sentence unlearned-lemmas score)
        updated-times-learned (update-times-learned (:conjugation-to-times-learned learning-progress) (:words sentence))
        updated-progress (->Learning-progress updated-times-learned updated-learning-order)
        updated-learning-database (update-learning-database-with-learned-lemmas updated-progress learning-database (:text-database learning-information) unlearned-lemmas)]
    (print-current-learning-status learning-information sentence score)
    (->Learning-information updated-progress updated-learning-database (:text-database learning-information) (:config learning-information))))

(defn learn-sentences [learning-information sentences scores]
  (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                   learning-information (map vector sentences scores)))

(defn get-a-unlearned-lemma [learning-information]
  (let [[lemma _] (->> learning-information :learning-database :lemmas-by-score first)]
    lemma))

(defn pop-top-sentence [learning-information]
  (if (not (empty? (->> learning-information :learning-database :sentences-by-score)))
   (let [[top-sentence top-sentence-score] (->> learning-information :learning-database :sentences-by-score first)
         remaining-sentences-by-score (->> learning-information :learning-database :sentences-by-score pop)]
     [top-sentence top-sentence-score remaining-sentences-by-score])
   (let [unlearned-lemma (get-a-unlearned-lemma learning-information)]
     (if (nil? unlearned-lemma) (throw (Exception. "Error: Trying to learn an unlearned lemma, when there are non left.")) nil)
     [(Sentence. (str "NoSentence: " (:raw unlearned-lemma)) [] [(Conjugation. (:raw unlearned-lemma)) #_(first (get (->> learning-information :text-database :lemma->conjugations) unlearned-lemma))]) nil
      (->> learning-information :learning-database :sentences-by-score)])))

(defn finished-learning? [learning-information]
  (let [total-lemma-count (->> learning-information :text-database :lemmas count)
        current-lemma-count (count-lemmas-learned learning-information)]
    (>= current-lemma-count total-lemma-count)))

(defn learn-top-sentence [learning-information]
  (if (finished-learning? learning-information) nil
   (let [[top-sentence top-sentence-score remaining-sentences-by-score] (pop-top-sentence learning-information)
         updated-database (assoc (:learning-database learning-information) :sentences-by-score remaining-sentences-by-score)
         updated-learning-information (assoc learning-information :learning-database updated-database)]
     (cond ; Sometimes all lemma have been learned in the top sentence, so we just skip it.
       (learnable? (:learning-progress learning-information) (:text-database learning-information) top-sentence)
       (learn-sentence updated-learning-information top-sentence top-sentence-score)
       :else
       (do (pprint (str "Could not be learned."))
           (pprint top-sentence)
           (pprint (sentence->lemmas (:text-database learning-information) top-sentence))
           (prn top-sentence)
           (throw (Exception. "Could not be learned."))
           (recur updated-learning-information))))))

(defn merge-frequencies [text-lemma-frequencies]
  (reducers/fold (partial merge-with +) text-lemma-frequencies))

(defn learn-all-lemmas [learning-information]
  (let [updated-learning-information (learn-top-sentence learning-information)]
    (if (not (nil? updated-learning-information))
      (recur updated-learning-information)
      learning-information)))

(defn text-database->new-learning-information [config text-database]
  (let [learning-progress (->Learning-progress {} [])
        learning-database (text-database->learning-database text-database)
        learning-information (->Learning-information learning-progress learning-database text-database config)]
    learning-information))

(defn directory->new-learning-information [config directory]
  (->> directory
       (lemmalearnerclj.textdatabase/directory->text-database config)
       (text-database->new-learning-information config)
       (#(assoc-in % [:text-database :texts] nil))))

(defn score-point-to-str [{:keys [lemma sentence score]}]
  (str (:raw lemma) " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))
