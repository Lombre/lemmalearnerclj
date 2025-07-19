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
   [clojure.java.io :as io]
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view])
  (:import
   [lemmalearnerclj.textdatastructures
    Paragraph
    Sentence
    Text]))

;; (java2d/run #(ui/label "Hello World!"))

(def config
  {:language "english"
   :should-print? true
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
                     :max-new-lemmas-per-sentence 1
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

(defn get-path-last-saved-learning-progress []
  (->> (clojure.java.io/file ".")
       file-seq
       (filter #(and (.isFile %)
                     (.endsWith (.getName %) ".saved")))
       (sort-by #(->> (str/split (.getName %) #"_") first))
       last
       .getAbsolutePath))

(def loaded-progress (learner/load-learning-progress initial-setup (get-path-last-saved-learning-progress)))

(def current-raw-learned-sentences (->> loaded-progress :learn-prog :learning-order (map #(->> % second second :raw))))


(defn make-text-bold [text]
  text
  ;; (str "<b>" text "</b>" )
  )

(defn print-current-learnable-sentences [learn-info]
  (->> (get-top-n-sentences learn-info 10)
       (#(for [i (range (count %))]
           (let [current-sentence (nth % i)
                 current-lemma (->> current-sentence (learner/sentence->unlearned-lemmas learn-info) first)]
             (str (+ i 1) ") "
                  (make-text-bold current-lemma)
                  " (f" ((->> learn-info :learn-db :lemma->frequency) current-lemma) ", s" (format "%.2f" (learner/score-sentence learn-info current-sentence)) ")"
                  ": "
                  (:raw current-sentence)
                  "\n\t"
                  ;; (vector  (map :raw  (learner/sentence->lemmas (:text-db learn-info) current-sentence)))
                  (learner/sentence->str-word-scores learn-info current-sentence)))))))

(defn learn-info->current-top-n-sentences-formated [n learn-info]
  (->> (get-top-n-sentences learn-info n)
       (#(for [i (range (count %))]
           (let [current-sentence (nth % i)
                 current-lemma (->> current-sentence (learner/sentence->unlearned-lemmas learn-info) first)]
             (str (+ i 1) ") "
                  (make-text-bold (clojure.string/capitalize current-lemma))
                  " (f" ((->> learn-info :learn-db :lemma->frequency) current-lemma) ", s" (format "%.2f" (learner/score-sentence learn-info current-sentence)) ")"
                  ": "
                  (:raw current-sentence)
                  "\n\t"
                  ;; (vector  (map :raw  (learner/sentence->lemmas (:text-db learn-info) current-sentence)))
                  (learner/sentence->str-word-scores learn-info current-sentence)))))))

(def *state
  (atom {:title "App title"
         :learning-info nil
         :save-path (get-path-last-saved-learning-progress)
         :texts-path path-to-texts
         :selected-learnable-sentence 0}))

(defn learned-sentences-view [{:keys [items]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :on-selected-items-changed (println "Changed selected")
   ;; :on-selected-changed (println "Chaned selected")
   :desc {:fx/type :list-view
          ;; :cell-factory {:fx/cell-type :list-cell
          ;;                :describe (fn [path]
          ;;                            {:text path})}
          :items items}
   ;; :items items
   }
  )

(defn learnable-sentences-view [{:keys [items selected]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :props {:on-selected-indices-changed (fn [x] (do (swap! *state assoc :selected-learnable-sentence (first x))
                                                    (println "Changed selected" x)))
           :selected-index selected}
   :desc {:fx/type :list-view
          :items items}})

(defn learn-selected-sentence [learning-info selected-learnable-sentence]
  (fn [_] (let [updated-learning-info (learner/learn-nth-top-sentence learning-info selected-learnable-sentence)]
            (swap! *state assoc :learning-info updated-learning-info))))




(defn root-window [{:keys [title learning-info selected-learnable-sentence save-path texts-path]}]
  {:fx/type :stage
   :showing true
   :title title
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children [ ;; Text path
                             {:fx/type :h-box
                              :children [{:fx/type :label
                                          :text "Path to texts:"}
                                         {:fx/type :text-field
                                          :on-text-changed #(swap! *state assoc :texts-path %)
                                          :text texts-path
                                          :h-box/hgrow :always}]}
                             ;; Save path
                             {:fx/type :h-box
                              :children [{:fx/type :label
                                          :text "Save file path:"}
                                         {:fx/type :text-field
                                          :on-text-changed #(swap! *state assoc :save-path %)
                                          :text save-path
                                          :h-box/hgrow :always}
                                         {:fx/type :button
                                          :on-action (fn [_] (swap! *state assoc :learning-info (learner/load-learning-progress initial-setup (get-path-last-saved-learning-progress))))
                                          :text "Load progess"}
                                         {:fx/type :button
                                          :on-action (fn [_] (learner/save-learning-progress learning-info))
                                          :text "Save progress"}
                                         ]}
                             ;; Buttons
                             {:fx/type :h-box
                              :children [{:fx/type :button
                                          :on-action (learn-selected-sentence learning-info selected-learnable-sentence)
                                          :text "Learn selected sentence"
                                          :h-box/hgrow :always}
                                         ]}
                             ;; Sentence lists
                             {:fx/type :h-box
                              :children [{:fx/type learned-sentences-view
                                          :items (->> learning-info :learn-prog :learning-order
                                                      (#(map str
                                                             (range 1 (count %)) (repeat ") ")
                                                             (map (fn [x] (->> x :lemmas (map clojure.string/capitalize) (clojure.string/join ", "))) %) (repeat ": ")
                                                             (map (fn [x] (->> x second second :raw)) %))))
                                          :h-box/hgrow :always}
                                         {:fx/type learnable-sentences-view
                                          :items (learn-info->current-top-n-sentences-formated 10 learning-info)
                                          :selected selected-learnable-sentence
                                          :h-box/hgrow :always}]}
                             ;; Conjugation lists
                             ]}}})

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root-window)))

(fx/mount-renderer *state renderer)

;; (defn start-everything []
;;   (update-loop nil loaded-progress))

;; (print-current-learnable-sentences loaded-progress)

;; (start-everything)
;; (println "foo")
;; (println "\u001b[31mbar\u001b[0m")
;; (println "baz")
;; (println "\u001b[2J")
;; (println "\u001b[3A:ddddd")
;; ;; (def updated-learn-info (update-lemmatization-and-reload loaded-progress "t" "not") )
