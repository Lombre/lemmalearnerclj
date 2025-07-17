(ns lemmalearnerclj.learner
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p]
   [lemmalearnerclj.helper :as helper])
  (:import
   [lemmalearnerclj.textdatastructures Sentence]))

(require '[clojure.data.priority-map :refer [priority-map priority-map-by]])

(defrecord Score-point [lemmas sentence score])

(defrecord Learning-progress [conj->#learned lemma->#learned learning-order])

(defrecord Learning-database [sentences-by-score lemmas-by-score lemma->frequency conjugation->frequency])

(defrecord Learning-information [learn-prog learn-db text-db config])

(defn is-nonsense-conjugation? [text-db conjugation]
  (nil? (get (:conjugation->lemma text-db) conjugation)))

(defn conjugation->lemma [text-db conjugation & {:keys [nilable] :or {nilable false}}]
  (if (not nilable)
    (getx (:conjugation->lemma text-db) conjugation )
    (get (:conjugation->lemma text-db) conjugation)))

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

(defn lemma->times-learned [learn-prog lemma]
  (->> learn-prog :lemma->#learned (#(get % lemma 0))))

(defn from-conjugations-lemma->times-learned
  ([learn-info lemma] (from-conjugations-lemma->times-learned (:learn-prog learn-info) (:text-db learn-info) lemma))
  ([learn-prog text-db lemma]
   (->> lemma
        (get (:lemma->conjugations text-db))
        (map #(conjugation->times-learned learn-prog %))
        (reduce +))))

(defn sentence->unlearned-lemmas [{text-db :text-db learn-prog :learn-prog} sentence]
  (->> (sentence->lemmas text-db sentence)
       (filter #(= 0 (lemma->times-learned learn-prog %)))))

(defn sentences->lemmas-by-frequency [text-db sentences]
  (->> sentences
       (pmap #(sentence->lemmas text-db %))
       flatten
       (p/frequencies)
       (clojure.core/reduce-kv #(assoc %1 %2 %3) (priority-map-by >)))) ; Transfer to priority map

(defn text-db->lemma->frequency [text-db]
  (sentences->lemmas-by-frequency text-db (:sentences text-db)))

(defn text-db->conjugation->frequency [text-db]
  (->> (:sentences text-db)
       (mapcat :words)
       p/frequencies
       (clojure.core/reduce-kv #(assoc %1 %2 %3) (priority-map-by >))))

(defn learnable? [learn-info sentence]
  (<= 1 (count (sentence->unlearned-lemmas learn-info sentence))
      (->> learn-info :config :learning-config :max-new-lemmas-per-sentence)))

(defn score-by-lemma-frequency
  ([{:keys [config learn-db learn-prog text-db]} lemma]
   #_-> (score-by-lemma-frequency (:learning-config config) (:lemma->frequency learn-db) text-db learn-prog lemma))
  ([learning-config lemma->frequency text-db learn-prog lemma]
   (let [times-learned (lemma->times-learned learn-prog lemma)]
     (if (> times-learned (getx learning-config :max-lemma-times-learned)) 0.0
         (* (/ (Math/log (lemma->frequency lemma 0.0)) (Math/log 2.0))
            (Math/pow (:drop-off-factor learning-config) times-learned ))))))

(defn score-by-lemma-and-conjugation-frequency
  ([{:keys [config learn-db learn-prog text-db]} conjugation]
   #_-> (score-by-lemma-and-conjugation-frequency (:learning-config config) (:lemma->frequency learn-db) (:conjugation->frequency learn-db) text-db learn-prog conjugation))
  ([learning-config lemma->frequency conjugation->frequency text-db learn-prog conjugation]
   (let [lemma (conjugation->lemma text-db conjugation :nilable true)
         lemma-times-learned (lemma->times-learned learn-prog lemma)
         conjugation-times-learned (conjugation->times-learned text-db conjugation)]
     (if (is-nonsense-conjugation? text-db conjugation) 0.0
         (+ (if (> lemma-times-learned (getx learning-config :max-lemma-times-learned)) 0.0
                (* (/ (Math/log (lemma->frequency lemma 0.0)) (Math/log 2.0))
                   (Math/pow (:drop-off-factor learning-config) lemma-times-learned)))
            (if (> conjugation-times-learned (getx learning-config :max-conjugation-times-learned)) 0.0
                (* (/ (Math/log 2.0) (Math/log 2.0))
                   (Math/pow (:drop-off-factor learning-config) lemma-times-learned))))))))

(defn score-sentence
  ([{config :config l-prog :learn-prog t-db :text-db l-db :learn-db} sentence]
   #_-> (score-sentence config (:lemma->frequency l-db) (:conjugation->frequency l-db) t-db l-prog sentence))
  ([config lemma->frequency conjugation->frequency text-db learn-prog sentence]
   (->> (:words sentence)
        ;; distinct
        (map #(score-by-lemma-and-conjugation-frequency (getx config :learning-config) lemma->frequency conjugation->frequency text-db learn-prog %))
        (reducers/reduce +)
        (#(/ % (count (sentence->unlearned-lemmas {:text-db text-db :learn-prog learn-prog} sentence )))))))

(defn sentences->sentences-by-score [learn-info sentences]
  (->> sentences
       (p-filter #(learnable? learn-info %))
       (reducers/reduce (fn [xs x] (assoc xs x (score-sentence learn-info x)))
                        (priority-map-by >))))

(defn text-db->learn-db [config text-db]
  (let [lemmas-by-frequency (text-db->lemma->frequency text-db)
        conjugations-by-frequency (text-db->conjugation->frequency text-db)
        lemmas-by-score lemmas-by-frequency
        sentences-by-score (sentences->sentences-by-score
                            (->Learning-information (->Learning-progress {} {} [])
                                                    {:lemma->frequency lemmas-by-frequency}
                                                    text-db
                                                    config)
                            (:sentences text-db))]
    (->Learning-database sentences-by-score lemmas-by-score lemmas-by-frequency conjugations-by-frequency)))

(defn update-with-sentence-pairs [learn-info sentence-learned-pairs]
  (let [learnable-sentences (filter #(nth % 1) sentence-learned-pairs)
        unlearnable-sentences (filter #(not (nth % 1)) sentence-learned-pairs)
        learnable-sentences-with-score (->> learnable-sentences
                                            (pmap #(identity [(nth % 0)
                                                              (nth % 1)
                                                              (score-sentence learn-info (nth % 0))])))]
    (->> (->> learn-info :learn-db :sentences-by-score)
         (#(reducers/reduce (fn [xs x] (dissoc xs (nth x 0))) % unlearnable-sentences))
         (#(reducers/reduce (fn [xs x] (assoc xs (nth x 0) (nth x 2))) % learnable-sentences-with-score)))))

(defn conjugation-learned-max-times? [learn-info conjugation]
  (let [max-times (->> learn-info :config :learning-config :max-conjugation-times-learned (+ 1))
        cur-times (get (->> learn-info :learn-prog :conj->#learned) conjugation 0)]
    (or (nil? cur-times) (< max-times cur-times))))

(defn lemma-learned-max-times? [learn-info lemma]
  (or (nil? lemma)
      (< (->> learn-info :config :learning-config :max-lemma-times-learned (+ 1))
         (lemma->times-learned (:learn-prog learn-info) lemma))))

(defn update-sentences-by-scores [learn-info sentence]
  (->> (sentence->lemmas (:text-db learn-info) sentence)
       (mapcat #(getx (->> learn-info :text-db :lemma->conjugations) %)) ;conjugations of all lemmas in sentence
       set
       (filter #(not (and (conjugation-learned-max-times? learn-info %)
                          (lemma-learned-max-times? learn-info (conjugation->lemma (:text-db learn-info) % :nilable true)))))
       (mapcat #(get (->> learn-info :text-db :conjugation->sentences ) %))
       #_set
       (pmap #(identity [% (learnable? learn-info %)]))
       (update-with-sentence-pairs learn-info)))

(defn update-learn-db-with-learned-sentence [{learn-db :learn-db text-db :text-db :as learn-info} sentence]
  (let [updated-sentences-by-scores (update-sentences-by-scores learn-info sentence)
        updated-lemmas-by-score (apply dissoc (:lemmas-by-score learn-db) (sentence->lemmas text-db sentence))]
    (->Learning-database updated-sentences-by-scores updated-lemmas-by-score (:lemma->frequency learn-db) (:conjugation-frequency learn-db))))

(defn update-lemma-times-learned [text-db lemma->#learned conj->#learned lemmas]
  (reducers/reduce #(assoc %1 %2 (->> %2
                                      (get (:lemma->conjugations text-db))
                                      (map conj->#learned)
                                      (filter some?)
                                      (reducers/reduce + 0)))
                   lemma->#learned lemmas))

(defn update-conj-times-learned [conj->#learned conjugations]
  (reducers/reduce #(update %1 %2 (fnil inc 0)) conj->#learned conjugations))

(defn update-learning-order
  "Append all the learned (word, sentence, score) pairs"
  [learn-info sentence unlearned-lemmas score]
  (conj  (->> learn-info :learn-prog :learning-order) (->Score-point unlearned-lemmas sentence score)))

(defn count-lemmas-learned [learn-info]
  (let [total-lemmas (->> learn-info :text-db :lemmas count)
        remaining-lemmas (->> learn-info :learn-db :lemmas-by-score count)]
    (- total-lemmas remaining-lemmas)))

(defn sentence->str-word-scores [learn-info sentence]
  (->> sentence
       :words
       (map #(if (not (contains? (->> learn-info :text-db :conjugation->lemma) %)) %
                 (let [lemma (conjugation->lemma (:text-db learn-info) %)]
                   (list [lemma
                          %
                          (lemma->times-learned (:learn-prog learn-info) lemma)
                          (get (->> learn-info :learn-prog :conj->#learned) %)
                          (get (->> learn-info :learn-db :lemma->frequency) lemma)
                          (get (->> learn-info :learn-db :conjugation->frequency) %)
                          (format "%.2f" (score-by-lemma-frequency learn-info lemma))]))))
       (str/join " " )))

(defn- print-current-learning-status [learn-info sentence ^Float score]
  (let [total-lemma-count (->> learn-info :text-db :lemmas count)
        current-lemma-count (+ 1 (count-lemmas-learned learn-info))
        unlearned-lemmas (sentence->unlearned-lemmas learn-info sentence)
        message (str current-lemma-count " of " total-lemma-count ", " (helper/make-text-bold (set unlearned-lemmas)) " "
                     (format "%.2f" (if (nil? score) 0.0 score)) " -> " (:raw sentence) "\n\t"
                     (sentence->str-word-scores learn-info sentence))]
    (print-if (or (zero? (mod current-lemma-count 100))
                  (>= 600  current-lemma-count)
                  (= current-lemma-count total-lemma-count)
                  (= current-lemma-count (->> learn-info :config :learning-config :max-lemmas-to-learn)))
              message)))

(defn learn-sentence
  ([^Learning-information learn-info sentence] (learn-sentence learn-info sentence (float (score-sentence learn-info sentence))))
  ([^Learning-information learn-info sentence ^Float score]
   ;; (pprint (sentence->unlearned-lemmas learn-info sentence))
   (let [unlearned-lemmas (sentence->unlearned-lemmas learn-info sentence)
         updated-learning-order (update-learning-order learn-info sentence unlearned-lemmas score)
         updated-conj-times-learned (update-conj-times-learned (:conj->#learned (:learn-prog learn-info)) (:words sentence))
         updated-lemma-times-learned (update-lemma-times-learned (:text-db learn-info)
                                                                 (->> learn-info :learn-prog :lemma->#learned)
                                                                 updated-conj-times-learned
                                                                 (sentence->lemmas (:text-db learn-info) sentence))
         updated-progress (->Learning-progress updated-conj-times-learned updated-lemma-times-learned updated-learning-order)
         updated-learn-db (update-learn-db-with-learned-sentence (assoc learn-info :learn-prog updated-progress) sentence)]
     (print-current-learning-status learn-info sentence score)
     (->Learning-information updated-progress updated-learn-db (:text-db learn-info) (:config learn-info)))))


(defn learn-sentences
  ([learn-info sentences] (reducers/reduce #(learn-sentence %1 %2) learn-info sentences))
  ([learn-info sentences scores] (reducers/reduce #(learn-sentence %1 (first %2) (second %2))
                                                  learn-info (map vector sentences scores))))

(defn get-an-unlearned-lemma [learn-info]
  (let [[lemma _] (->> learn-info :learn-db :lemmas-by-score first)]
    lemma))

(defn get-top-n-sentences [learn-info n]
  (->> (->> learn-info :learn-db :sentences-by-score)
       (take n)
       (map first)))

(defn make-fake-sentence-with-an-unlearned-lemma [learn-info]
  (let [unlearned-lemma (get-an-unlearned-lemma learn-info)
        unlearned-lemma-sentence (Sentence. (str "NoSentence: " unlearned-lemma)
                                                []
                                                [unlearned-lemma ]) ]
        (if (not (nil? unlearned-lemma)) nil
            (throw (Exception. "Error: Trying to learn an unlearned lemma, when there are non left.")))
        [unlearned-lemma-sentence 0.0 learn-info]))

(defn pop-nth-top-sentence [learn-info n]
  (let [sentences-by-score (->> learn-info :learn-db :sentences-by-score)]
    (if (not (empty? sentences-by-score))
      (let [[top-sentence top-sentence-score] (last (take (+ n 1) sentences-by-score))
            updated-learn-info (update-in learn-info [:learn-db :sentences-by-score] #(dissoc % top-sentence))]
        [top-sentence top-sentence-score updated-learn-info])
      (make-fake-sentence-with-an-unlearned-lemma learn-info))))

(defn pop-top-sentence [learn-info]
  (pop-nth-top-sentence 0 learn-info))

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
      (throw (Exception. (str "Could not not learn sentence: " (:raw sentence))))))

(defn learn-nth-top-sentence [learn-info n]
  (if (finished-learning? learn-info) nil
      (let [[top-sentence top-sentence-score updated-learn-info] (pop-nth-top-sentence learn-info n)]
        (if (learnable? learn-info top-sentence)
          (learn-sentence updated-learn-info top-sentence top-sentence-score)
          (throw-learning-error learn-info top-sentence)))))

(defn learn-top-sentence [learn-info]
  (learn-nth-top-sentence learn-info 0))

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
   (let [learn-prog (->Learning-progress {} {} [])
         learn-db (text-db->learn-db config text-db)
         learn-info (->Learning-information learn-prog learn-db text-db config)]
     learn-info)
   (str "Finished preparing for learning")))

(defn directory->new-learn-info [config directory]
  (->> directory
       (lemmalearnerclj.textdatabase/directory->text-db config)
       (text-db->new-learn-info config)))

(defn score-point-to-str [{:keys [lemma sentence score]}]
  (str lemma " " (if (nil? score) score (math/round (- score))) " -> " (:raw sentence)))

(defn save-learning-progress
  ([learn-info] (save-learning-progress (str (getx (->> learn-info :config) :start-time)
                                             "-"
                                             (getx (->> learn-info :config) :save-path)) learn-info))
  ([path learn-info]
   (spit path (->> learn-info :learn-prog :learning-order
                   (map :sentence)
                   (map :raw)
                   (str/join "\n")))))


(defn load-learning-progress [learning-progress path]
  (->> (if (and (some? path) (.exists (io/file path)))
         (->> (slurp path)
              (#(str/split % #"\n"))
              (map #(parser/parse-raw-paragraph (->> learning-progress :config :parsing-config) %))
              (map #(->> % :sentences first))
              (textdatabase/sentences->sentences-with-lemmas (->> learning-progress :text-db :conjugation->lemma)))
         [])
       (learn-sentences learning-progress)))
