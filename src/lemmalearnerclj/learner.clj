(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.math :as math]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p])
  (:import
   [lemmalearnerclj.textdatastructures Conjugation Sentence]))

(require '[clojure.data.priority-map :refer [priority-map]])

(defrecord Score-point [lemma sentence score])

(defrecord Learning-progress [conjugation-to-times-learned learning-order])

(defrecord Learning-database [sentences-by-score lemmas-by-score lemma->frequency])

(defrecord Learning-information [learning-progress learning-database text-database config])

(defn conjugation->lemma [text-database conjugation]
  (get (:conjugation->lemma text-database) conjugation ))

;; (defn sentence->lemmas [text-database sentence]
;;   (->> sentence
;;        :words
;;        (map #(conjugation->lemma text-database %))
;;        (filter some?)
;;        distinct))

(defn sentence->lemmas [text-database sentence]
  (if (contains? sentence :lemmas)
    (:lemmas sentence)
    (->> (:words sentence)
         (map #(conjugation->lemma text-database %))
         (filter some?)
         distinct)))

(defn lemma->sentences [text-database lemma]
  (->> lemma
       (get (:lemma->conjugations text-database) )
       (mapcat #(get (:conjugation->sentences text-database) %))
       set))

(defn conjugation->times-learned [learning-progress word]
  (get (:conjugation-to-times-learned learning-progress) word 0))

(defn lemma->times-learned
  ([learning-information lemma] (lemma->times-learned (:learning-progress learning-information) (:text-database learning-information) lemma))
  ([learning-progress text-database lemma]
   (->> lemma
        (get (:lemma->conjugations text-database))
        (map #(conjugation->times-learned learning-progress %))
        (reduce +))))

(defn sentence->unlearned-lemmas [{text-database :text-database learning-progress :learning-progress} sentence]
  (if (not= (:raw sentence) "NoSentence: rage") nil
      (do  (pprint (sentence->lemmas text-database sentence))
           (pprint (lemma->times-learned learning-progress text-database (first (sentence->lemmas text-database sentence))))
           (pprint (->> (sentence->lemmas text-database sentence)
                        (filter #(= 0 (lemma->times-learned learning-progress text-database %)))))))
  (->> (sentence->lemmas text-database sentence)
       (filter #(= 0 (lemma->times-learned learning-progress text-database %)))))

(defn sentences->lemmas-by-frequency [text-database sentences]
  (->> sentences
       (pmap #(sentence->lemmas text-database %))
       flatten
       (p/frequencies)
       (clojure.core/reduce-kv #(assoc %1 %2 (- %3)) (priority-map)))) ; Transfer to priority map

(defn text-database->lemma->frequency [text-database]
  (sentences->lemmas-by-frequency text-database (:sentences text-database)))

(defn learnable? [learning-information sentence]
  (= (count (sentence->unlearned-lemmas learning-information sentence))
     1))

(defn score-by-lemma-frequency [lemma->frequency text-database learning-progress lemma]
  (* (get lemma->frequency lemma 0.0)
     (Math/pow 0.25 (lemma->times-learned learning-progress text-database lemma))))

;; (defn score-by-unlearned-lemma-frequency [lemma->frequency text-database learning-progress lemma]
;;   (if (nil? lemma) 0.0
;;       (if (= 0 (lemma->times-learned learning-progress text-database lemma))
;;         (get lemma->frequency lemma 0)
;;         0.0)))

(defn score-sentence
  ([{l-prog :learning-progress t-db :text-database l-db :learning-database} sentence]
   #_-> (score-sentence (:lemma->frequency l-db) t-db l-prog sentence))
  ([lemma->frequency text-database learning-progress sentence]
   (->> (sentence->lemmas text-database sentence)
        (map #(score-by-lemma-frequency lemma->frequency text-database learning-progress %))
        (reducers/reduce +))))

(defn sentences->sentences-by-score [learning-information sentences]
  (->> sentences
       (helper/p-filter #(learnable? learning-information %))
       (reducers/reduce (fn [xs x] (assoc xs x (score-sentence learning-information x)))
                        (priority-map))))

(defn text-database->learning-database [config text-database]
  (let [lemmas-by-frequency (text-database->lemma->frequency text-database)
        lemmas-by-score lemmas-by-frequency
        sentences-by-score (sentences->sentences-by-score (->Learning-information (->Learning-progress {} [])
                                                                                  {:lemma->frequency lemmas-by-frequency}
                                                                                  text-database
                                                                                  config)
                                                          (:sentences text-database))]
    (->Learning-database sentences-by-score lemmas-by-score lemmas-by-frequency)))

(defn get-actual-unlearnable-sentences [unlearnable-sentences sentences-by-score]
  (->> unlearnable-sentences
       (p/group-by #(contains? sentences-by-score (nth % 0 )))
       (#(get % true))))

(defn update-with-sentence-pairs [learning-information sentence-learned-pairs]
  (let [learnable-sentences (filter #(nth % 1) sentence-learned-pairs)
        unlearnable-sentences (filter #(not (nth % 1)) sentence-learned-pairs)
        learnable-sentences-with-score (->> learnable-sentences
                                            (pmap #(identity [(nth % 0)
                                                             (nth % 1)
                                                             (score-sentence learning-information (nth % 0))])))]
    (->> (->> learning-information :learning-database :sentences-by-score)
         (#(reducers/reduce (fn [xs x] (dissoc xs x)) % (map (fn [x] (nth x 0)) unlearnable-sentences)))
         (#(reducers/reduce (fn [xs x] (assoc xs (nth x 0) (nth x 2))) % learnable-sentences-with-score)))))

(defn update-sentences-by-scores [learning-information sentence]
  (->> (sentence->lemmas (:text-database learning-information) sentence)
       (filter #(>= 3 (lemma->times-learned learning-information %)))
       (mapcat #(lemma->sentences (:text-database learning-information) %))
       set
       (map #(identity [% (learnable? learning-information %)]))
       (update-with-sentence-pairs learning-information)))

(defn update-learning-database-with-learned-sentence [{learning-database :learning-database :as  learning-information} sentence]
  (let [updated-sentences-by-scores (update-sentences-by-scores learning-information sentence)
        updated-lemmas-by-score (apply dissoc (:lemmas-by-score learning-database) (sentence->lemmas (:text-database learning-information) sentence))]
    (->Learning-database updated-sentences-by-scores updated-lemmas-by-score (:lemma->frequency learning-database))))

(defn update-times-learned [conjugation-to-times-learned conjugations]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) conjugation-to-times-learned conjugations))

(defn update-learning-order
  "Append all the learned (word, sentence, score) pairs"
  [learning-information sentence unlearned-lemmas score]
  (reducers/reduce #(conj %1 (->Score-point %2 sentence score)) (->> learning-information :learning-progress :learning-order) unlearned-lemmas ))

(defn count-lemmas-learned [learning-information]
  (->> learning-information :learning-progress :learning-order count))

(defn- print-current-learning-status [learning-information sentence ^Float score]
  (let [total-lemma-count (->> learning-information :text-database :lemmas count)
        current-lemma-count (+ 1 (count-lemmas-learned learning-information))
        unlearned-lemmas (sentence->unlearned-lemmas learning-information sentence)
        message (str current-lemma-count " of " total-lemma-count ", " (set (map :raw unlearned-lemmas)) " "
                     (format "%.2f" (if (nil? score) 0.0 score)) " -> " (:raw sentence))]
    (helper/print-if (or (zero? (mod current-lemma-count 1))
                         (>= 100  current-lemma-count)
                         (= current-lemma-count total-lemma-count))
                     message)))

(defn learn-sentence [^Learning-information learning-information sentence ^Float score]
  (let [unlearned-lemmas (sentence->unlearned-lemmas learning-information sentence)
        updated-learning-order (update-learning-order learning-information sentence unlearned-lemmas score)
        updated-times-learned (update-times-learned (:conjugation-to-times-learned (:learning-progress learning-information)) (:words sentence))
        updated-progress (->Learning-progress updated-times-learned updated-learning-order)
        updated-learning-database (update-learning-database-with-learned-sentence (assoc learning-information :learning-progress updated-progress) sentence)]
    (print-current-learning-status learning-information sentence score)
    (->Learning-information updated-progress updated-learning-database (:text-database learning-information) (:config learning-information))))

(defn learn-sentences [learning-information sentences scores]
  (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                   learning-information (map vector sentences scores)))

(defn get-a-unlearned-lemma [learning-information]
  (let [[lemma _] (->> learning-information :learning-database :lemmas-by-score first)]
    lemma))

(defn pop-top-sentence [learning-information]
  (let [sentences-by-score (->> learning-information :learning-database :sentences-by-score)]
    (if (not (empty? sentences-by-score))
      (let [[top-sentence top-sentence-score] (first sentences-by-score)
            updated-learning-information (update-in learning-information [:learning-database :sentences-by-score] pop)]
        [top-sentence top-sentence-score updated-learning-information])

      (let [unlearned-lemma (get-a-unlearned-lemma learning-information)
            unlearned-lemma-sentence (Sentence. (str "NoSentence: " (:raw unlearned-lemma)) [] [(Conjugation. (:raw unlearned-lemma)) ]) ]
        (if (nil? unlearned-lemma) (throw (Exception. "Error: Trying to learn an unlearned lemma, when there are non left.")) nil)
        [unlearned-lemma-sentence 0.0 learning-information]))))

(defn finished-learning? [learning-information]
  (let [total-lemma-count (->> learning-information :text-database :lemmas count)
        current-lemma-count (count-lemmas-learned learning-information)]
    (>= current-lemma-count total-lemma-count)))

(defn- throw-learning-error [learning-information sentence]
  (do (println "Could not learn sentence")
      (pprint sentence)
      (pprint (sentence->lemmas (:text-database learning-information) sentence))
      (pprint (sentence->unlearned-lemmas learning-information sentence))
      (throw (Exception. (str "Could not not learn sentence: " (prn sentence))))))

(defn learn-top-sentence [learning-information]
  (if (finished-learning? learning-information) nil
      (let [[top-sentence top-sentence-score updated-learning-information]
            #_-> (pop-top-sentence learning-information)]
     (if (learnable? learning-information top-sentence)
       (learn-sentence updated-learning-information top-sentence top-sentence-score)
       (throw-learning-error learning-information top-sentence)))))

(defn merge-frequencies [text-lemma-frequencies]
  (reducers/fold (partial merge-with +) text-lemma-frequencies))

(defn learn-all-lemmas [learning-information]
  (helper/print-if (empty? (->> learning-information :learning-progress :learning-order)) "Started learning.")
  (let [updated-learning-information (learn-top-sentence learning-information)]
    (if (not (nil? updated-learning-information))
      (recur updated-learning-information)
      (do (println "Finished learning.")
          learning-information))))

(defn text-database->new-learning-information [config text-database]
  (helper/wrap-with-print
   (str "Started preparing for learning.")
   (let [learning-progress (->Learning-progress {} [])
         learning-database (text-database->learning-database config text-database)
         learning-information (->Learning-information learning-progress learning-database text-database config)]
     learning-information)
   (str "Finished preparing for learning")))

(defn directory->new-learning-information [config directory]
  (->> directory
       (lemmalearnerclj.textdatabase/directory->text-database config)
       (text-database->new-learning-information config)))

(defn score-point-to-str [{:keys [lemma sentence score]}]
  (str (:raw lemma) " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))
