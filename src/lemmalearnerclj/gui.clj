(ns lemmalearnerclj.gui
  (:require
   [clojure.core.match :refer [match]]
   [clojure.core.reducers :as reducers]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [lemmalearnerclj.learner :as learner]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatabase :refer :all]
   [lemmalearnerclj.textdatabase :as textdatabase]
   [lemmalearnerclj.textdatastructures :refer :all]
   [clojure.java.io :as io])
  (:import
   [lemmalearnerclj.textdatastructures
    Paragraph
    Sentence
    Text]))


;; (java2d/run #(ui/label "Hello World!"))

(def config
  {:language "english"
   :parsing-config {:punctuation #{\. \! \?}
                    :quote-pairs {\" \"
                                  \“ \”
                                  \' \'
                                  \( \)
                                  \[ \]
                                  \¿ \?
                                  \« \»
                                  \¡ \!}
                    :other-punctuation #{\. \; \:}}
   :learning-config {:drop-off-factor 0.5
                     :max-lemma-times-learned 5
                     :max-conjugation-times-learned 2
                     :max-lemmas-to-learn 24000}
   :save-path "test-learning-order.saved"
   :start-time (str/replace (.toString (.withNano (java.time.LocalDateTime/now) 0)) #":" ";")})

(def path-to-texts (str "texts/" (:language config) "/"))

(defn initialize []
  (->> (directory->text-db config path-to-texts)
       (learner/text-db->new-learn-info config)))

(defn get-top-n-sentences [learn-info, n]
  (learner/get-top-n-sentences learn-info n))


(def initial-setup (initialize))


;; (def kage1 (if (not (boolean (resolve 'kage1)))
;;              (initialize)
;;              @(resolve 'kage1)))

(def learned-first-sentence
  (learner/learn-top-sentence initial-setup))

(defn make-text-bold [text]
  (str "\u001b[1m" text "\u001b[22m" ))

(defn print-current-learnable-sentences [learn-info]
  (->> (get-top-n-sentences learn-info 10)
       (#(for [i (range (count %))]
           (let [current-sentence (nth % i)
                 current-lemma (->> current-sentence (learner/sentence->unlearned-lemmas learn-info) first :raw)]
             (println (str (+ i 1) ") "
                           (make-text-bold current-lemma)
                           " (" ((->> learn-info :learn-db :lemma->frequency) current-lemma) ")"
                           ": "
                           (:raw current-sentence)
                           " -> "
                           (->> current-sentence (learner/sentence->lemmas (:text-db learn-info))
                                (map :raw)
                                vector)
                           ;; (vector  (map :raw  (learner/sentence->lemmas (:text-db learn-info) current-sentence)))
                           "\n\t\t"
                           (learner/sentence->str-word-scores learn-info current-sentence))))))))

(defn print-initial-setup [learn-info]
  (print-current-learnable-sentences learn-info))

(print-initial-setup initial-setup)

(defn tui-update [old-state current-state]
  (do (println)
      (let [{sentence :sentence score :score lemma :lemma} (->> current-state :learn-prog :learning-order last)]
        (println (str "Learned \"" (:raw lemma) "\"" " from sentence: " (:raw sentence))))
      (doall (print-current-learnable-sentences current-state))))

(defn get-new-action []
  (let [input (read-line)
        split-input (str/split input #" ")]
    (match [split-input]
           ;; learn sentence
           [["learn" (learn-val :guard #(re-matches #"-?\d+" %))]] [:learn (- (Integer/parseInt learn-val) 1)]
           [[(learn-val :guard #(re-matches #"-?\d+" %))]] [:learn (- (Integer/parseInt learn-val) 1)]
           [[(:or "quit" "q")]] [:quit]
           ;; print status
           [["print"]] [:print]
           :else (do (println "Invalid action, try again")
                     (recur)))))

(defn- print-current-state [current-state]
  (do (print-current-learnable-sentences current-state)))

(defn action->updated-state [current-state action]
  (case (first action)
    :learn (learner/learn-nth-top-sentence current-state (second action))
    :quit nil
    :print (do (print-current-state current-state)
               current-state)
    (throw (Exception. (str "Unhandeled action: " action)))))

(defn update-loop [old-state current-state]
  (do (tui-update old-state current-state) ; Continusly update state based on an action
      (->> (get-new-action)
           (action->updated-state current-state)
           (#(if (nil? %) current-state
                 (do (learner/save-learning-progress %)
                     (update-loop current-state %)))))))

(defn- print-welcome-message [initial-setup]
  (println "Hello!"))

(defn start-tui []
  (do (print-welcome-message initial-setup)
      ;; (learner/load-learning-progress)
      (update-loop nil initial-setup)))

(defn get-path-last-saved-learning-progress []
  (->> (clojure.java.io/file ".")
       file-seq
       (filter #(and (.isFile %) (.endsWith (.getName %) ".saved")))
       (map #(.getName %))
       (sort-by #(->> (str/split % #"_") first))
       last))

(->> (get-path-last-saved-learning-progress)
     (learner/load-learning-progress initial-setup)
     ;; (update-loop nil)
     )

(defn update-lemmatization [learning-information conjugation new-lemma]
  (let [old-lemmatization (get (->> learning-information :text-db :conjugation->lemma) conjugation)]
    (pprint/pprint old-lemmatization)
    old-lemmatization))

;; Change lemmatization
(let [conjugation "cakes"
      new-lemma "cake"]
  (update-lemmatization initial-setup conjugation new-lemma))

(println "kage")
