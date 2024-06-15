(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.math :as math]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p])
  (:import
   [lemmalearnerclj.textdatastructures Conjugation Sentence]))

(require '[clojure.data.priority-map :refer [priority-map priority-map-by]])

(defrecord Score-point [lemma sentence score])

(defrecord Learning-progress [conj->#learned learning-order])

(defrecord Learning-database [sentences-by-score lemmas-by-score lemma->frequency])

(defrecord Learning-information [learn-prog learn-db text-db config])

(defn conjugation->lemma [text-db conjugation]
  (getx (:conjugation->lemma text-db) conjugation ))

(defn sentence->lemmas [text-db sentence]
  (if (contains? sentence :lemmas)
    (:lemmas sentence)
    (->> (:words sentence)
         (map #(conjugation->lemma text-db %))
         (filter some?)
         distinct)))

(defn lemma->sentences [text-db lemma]
  (->> lemma
       (getx (:lemma->conjugations text-db))
       (mapcat #(get (:conjugation->sentences text-db) %))
       set))

(defn conjugation->times-learned [learn-prog word]
  (get (:conj->#learned learn-prog) word 0))

(defn lemma->times-learned
  ([learn-info lemma] (lemma->times-learned (:learn-prog learn-info) (:text-db learn-info) lemma))
  ([learn-prog text-db lemma]
   (->> lemma
        (getx (:lemma->conjugations text-db))
        (map #(conjugation->times-learned learn-prog %))
        (reduce +))))

(defn sentence->unlearned-lemmas [{text-db :text-db learn-prog :learn-prog} sentence]
  (if (not= (:raw sentence) "NoSentence: rage") nil
      (do  (pprint (sentence->lemmas text-db sentence))
           (pprint (lemma->times-learned learn-prog text-db (first (sentence->lemmas text-db sentence))))
           (pprint (->> (sentence->lemmas text-db sentence)
                        (filter #(= 0 (lemma->times-learned learn-prog text-db %)))))))
  (->> (sentence->lemmas text-db sentence)
       (filter #(= 0 (lemma->times-learned learn-prog text-db %)))))

(defn sentences->lemmas-by-frequency [text-db sentences]
  (->> sentences
       (pmap #(sentence->lemmas text-db %))
       flatten
       (p/frequencies)
       (clojure.core/reduce-kv #(assoc %1 %2 %3) (priority-map-by >)))) ; Transfer to priority map

(defn text-db->lemma->frequency [text-db]
  (sentences->lemmas-by-frequency text-db (:sentences text-db)))

(defn learnable? [learn-info sentence]
  (= (count (sentence->unlearned-lemmas learn-info sentence))
     1))

(defn score-by-lemma-frequency
  ([{:keys [config learn-db learn-prog text-db]} lemma]
   #_-> (score-by-lemma-frequency (:learning-config config) (:lemma->frequency learn-db) text-db learn-prog lemma))
  ([learning-config lemma->frequency text-db learn-prog lemma]
   (let [times-learned (lemma->times-learned learn-prog text-db lemma)]
     (if (> times-learned (getx learning-config :max-lemma-times-learned)) 0.0
         (* (/ (Math/log (lemma->frequency lemma 0.0)) (Math/log 2.0))
            (Math/pow (:drop-off-factor learning-config) times-learned ))))))

(defn score-sentence
  ([{config :config l-prog :learn-prog t-db :text-db l-db :learn-db} sentence]
   #_-> (score-sentence config (:lemma->frequency l-db) t-db l-prog sentence))
  ([config lemma->frequency text-db learn-prog sentence]
   (->> (sentence->lemmas text-db sentence)
        (map #(score-by-lemma-frequency (getx config :learning-config) lemma->frequency text-db learn-prog %))
        (reducers/reduce +))))

(defn sentences->sentences-by-score [learn-info sentences]
  (->> sentences
       (p-filter #(learnable? learn-info %))
       (reducers/reduce (fn [xs x] (assoc xs x (score-sentence learn-info x)))
                        (priority-map-by >))))

(defn text-db->learn-db [config text-db]
  (let [lemmas-by-frequency (text-db->lemma->frequency text-db)
        lemmas-by-score lemmas-by-frequency
        sentences-by-score (sentences->sentences-by-score
                            (->Learning-information (->Learning-progress {} [])
                                                    {:lemma->frequency lemmas-by-frequency}
                                                    text-db
                                                    config)
                            (:sentences text-db))]
    (->Learning-database sentences-by-score lemmas-by-score lemmas-by-frequency)))

(defn update-with-sentence-pairs [learn-info sentence-learned-pairs]
  (let [learnable-sentences (filter #(nth % 1) sentence-learned-pairs)
        unlearnable-sentences (filter #(not (nth % 1)) sentence-learned-pairs)
        learnable-sentences-with-score (->> learnable-sentences
                                            (pmap #(identity [(nth % 0)
                                                              (nth % 1)
                                                              (score-sentence learn-info (nth % 0))])))]
    (->> (->> learn-info :learn-db :sentences-by-score)
         (#(reducers/reduce (fn [xs x] (dissoc xs x)) % (pmap (fn [x] (nth x 0)) unlearnable-sentences)))
         (#(reducers/reduce (fn [xs x] (assoc xs (nth x 0) (nth x 2))) % learnable-sentences-with-score)))))

(defn update-sentences-by-scores [learn-info sentence]
  (->> (sentence->lemmas (:text-db learn-info) sentence)
       (filter #(>= (->> learn-info :config :learning-config :max-lemma-times-learned (+ 1))
                    (lemma->times-learned learn-info %)))
       (mapcat #(lemma->sentences (:text-db learn-info) %))
       set
       (pmap #(identity [% (learnable? learn-info %)]))
       (update-with-sentence-pairs learn-info)))

(defn update-learn-db-with-learned-sentence [{learn-db :learn-db text-db :text-db :as learn-info} sentence]
  (let [updated-sentences-by-scores (update-sentences-by-scores learn-info sentence)
        updated-lemmas-by-score (apply dissoc (:lemmas-by-score learn-db) (sentence->lemmas text-db sentence))]
    (->Learning-database updated-sentences-by-scores updated-lemmas-by-score (:lemma->frequency learn-db))))

(defn update-times-learned [conj->#learned conjugations]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) conj->#learned conjugations))

(defn update-learning-order
  "Append all the learned (word, sentence, score) pairs"
  [learn-info sentence unlearned-lemmas score]
  (reducers/reduce #(conj %1 (->Score-point %2 sentence score)) (->> learn-info :learn-prog :learning-order) unlearned-lemmas ))

(defn count-lemmas-learned [learn-info]
  (->> learn-info :learn-prog :learning-order count))

(defn- print-current-learning-status [learn-info sentence ^Float score]
  (let [total-lemma-count (->> learn-info :text-db :lemmas count)
        current-lemma-count (+ 1 (count-lemmas-learned learn-info))
        unlearned-lemmas (sentence->unlearned-lemmas learn-info sentence)
        message (str current-lemma-count " of " total-lemma-count ", " (set (map :raw unlearned-lemmas)) " "
                     (format "%.2f" (if (nil? score) 0.0 score)) " -> " (:raw sentence) "\n\t"
                     (str/join " " (map #(list [(:raw %)
                                                (lemma->times-learned learn-info %)
                                                (getx (->> learn-info :learn-db :lemma->frequency) %)
                                                (format "%.2f" (score-by-lemma-frequency learn-info %))])
                                        (sentence->lemmas (:text-db learn-info) sentence))))]
    (print-if (or (zero? (mod current-lemma-count 100))
                  (>= 600  current-lemma-count)
                  (= current-lemma-count total-lemma-count)
                  (= current-lemma-count (->> learn-info :config :learning-config :max-lemmas-to-learn)))
                     message)))

(defn learn-sentence [^Learning-information learn-info sentence ^Float score]
  (let [unlearned-lemmas (sentence->unlearned-lemmas learn-info sentence)
        updated-learning-order (update-learning-order learn-info sentence unlearned-lemmas score)
        updated-times-learned (update-times-learned (:conj->#learned (:learn-prog learn-info)) (:words sentence))
        updated-progress (->Learning-progress updated-times-learned updated-learning-order)
        updated-learn-db (update-learn-db-with-learned-sentence (assoc learn-info :learn-prog updated-progress) sentence)]
    (print-current-learning-status learn-info sentence score)
    (->Learning-information updated-progress updated-learn-db (:text-db learn-info) (:config learn-info))))

(defn learn-sentences [learn-info sentences scores]
  (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                   learn-info (map vector sentences scores)))

(defn get-a-unlearned-lemma [learn-info]
  (let [[lemma _] (->> learn-info :learn-db :lemmas-by-score first)]
    lemma))

(defn pop-top-sentence [learn-info]
  (let [sentences-by-score (->> learn-info :learn-db :sentences-by-score)]
    (if (not (empty? sentences-by-score))
      (let [[top-sentence top-sentence-score] (first sentences-by-score)
            updated-learn-info (update-in learn-info [:learn-db :sentences-by-score] pop)]
        [top-sentence top-sentence-score updated-learn-info])

      (let [unlearned-lemma (get-a-unlearned-lemma learn-info)
            unlearned-lemma-sentence (Sentence. (str "NoSentence: " (:raw unlearned-lemma))
                                                []
                                                [(Conjugation. (:raw unlearned-lemma)) ]) ]
        (if (not (nil? unlearned-lemma)) nil
          (throw (Exception. "Error: Trying to learn an unlearned lemma, when there are non left.")))
        [unlearned-lemma-sentence 0.0 learn-info]))))

(defn finished-learning? [learn-info]
  (let [total-lemma-count (->> learn-info :text-db :lemmas count)
        current-lemma-count (count-lemmas-learned learn-info)]
    (or (>= current-lemma-count total-lemma-count)
        (>= current-lemma-count (->> learn-info :config :learning-config :max-lemmas-to-learn)))))

(defn- throw-learning-error [learn-info sentence]
  (do (println "Could not learn sentence")
      (pprint sentence)
      (pprint (sentence->lemmas (:text-db learn-info) sentence))
      (pprint (sentence->unlearned-lemmas learn-info sentence))
      (throw (Exception. (str "Could not not learn sentence: " (prn sentence))))))

(defn learn-top-sentence [learn-info]
  (if (finished-learning? learn-info) nil
      (let [[top-sentence top-sentence-score updated-learn-info] (pop-top-sentence learn-info)]
        (if (learnable? learn-info top-sentence)
          (learn-sentence updated-learn-info top-sentence top-sentence-score)
          (throw-learning-error learn-info top-sentence)))))

(defn merge-frequencies [text-lemma-frequencies]
  (reducers/fold (partial merge-with +) text-lemma-frequencies))

(defn learn-all-lemmas [learn-info]
  (print-if (empty? (->> learn-info :learn-prog :learning-order)) "Started learning.")
  (let [updated-learn-info (learn-top-sentence learn-info)]
    (if (not (nil? updated-learn-info))
      (recur updated-learn-info)
      (do (println "Finished learning.")
          learn-info))))

(defn text-db->new-learn-info [config text-db]
  (wrap-with-print
   (str "Started preparing for learning.")
   (let [learn-prog (->Learning-progress {} [])
         learn-db (text-db->learn-db config text-db)
         learn-info (->Learning-information learn-prog learn-db text-db config)]
     learn-info)
   (str "Finished preparing for learning")))

(defn directory->new-learn-info [config directory]
  (->> directory
       (lemmalearnerclj.textdatabase/directory->text-db config)
       (text-db->new-learn-info config)))

(defn score-point-to-str [{:keys [lemma sentence score]}]
  (str (:raw lemma) " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))
