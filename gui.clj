(ns lemmalearnerclj.gui
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.core.reducers :as reducers]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.learner :as learner]
   [lemmalearnerclj.textdatabase :refer :all]
   [lemmalearnerclj.textdatastructures :refer :all]
   [clojure.set :as set]
   [parallel.core :as p])
  ;; (:import [javaaa helpermethods])
  )

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

(defn get-path-last-saved-learning-progress []
  (->> (clojure.java.io/file ".")
       file-seq
       (filter #(and (.isFile %)
                     (.endsWith (.getName %) ".saved")))
       (sort-by #(->> (str/split (.getName %) #"_") first))
       last
       .getAbsolutePath))

(defn make-text-bold [text] text)

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
;; (def learning-info-storage nil)

(def learning-info-storage
  (when-not (nil? (resolve '*state)) (:learning-info (deref (eval (symbol (resolve '*state))))))) ; static initialization of :learning-info, for easier testing. nil first time this is run.
(def *state
  (atom {:title "App title"
         :learning-info learning-info-storage ; Only initialize once (static), for easier testing
         :save-path (get-path-last-saved-learning-progress)
         :texts-path path-to-texts
         :selected-conjugation "cakes"
         :selected-lemmatization "cake"
         :selected-learnable-sentence 0}))
;; (def learning-info-storage nil)

(defn learned-sentences-view [{:keys [items]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :on-selected-items-changed (println "Changed selected")
   :desc {:fx/type :list-view
          :items items}})

(defn learnable-sentences-view [{:keys [items selected]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :props {:on-selected-indices-changed
           (fn [x] (do (swap! *state assoc :selected-learnable-sentence (first x))
                       (println "Changed selected" x)
                       ;; Update lemmatizaiton fields
                       (let [selected-sentence (nth (learner/get-top-n-sentences (:learning-info @*state) (inc (first x))) (first x))
                             unlearned-lemma (first (learner/sentence->unlearned-lemmas (:learning-info @*state) selected-sentence))
                             unlearned-conjugation (->> selected-sentence :words
                                                        (filter #(= (learner/conjugation->lemma (->> @*state :learning-info :text-db) % :nilable true) unlearned-lemma))
                                                        first)]
                         (swap! *state assoc :selected-conjugation (if (nil? unlearned-conjugation) "" unlearned-conjugation))
                         (swap! *state assoc :selected-lemmatization (if (nil? unlearned-lemma) "" unlearned-lemma)))))
           :selected-index selected}
   :desc {:fx/type :list-view
          :items items}})

(defn learn-selected-sentence [learning-info selected-learnable-sentence]
  (fn [_] (let [updated-learning-info (learner/learn-nth-top-sentence learning-info selected-learnable-sentence)]
            (swap! *state assoc :learning-info updated-learning-info))))

(defn- gui-conjugation-lists [learning-info selected-conjugation selected-lemmatization]
  {:fx/type :h-box
   :children [{:fx/type :label
               :text "Conjugation: "}
              {:fx/type :text-field
               :on-text-changed #(swap! *state assoc :selected-conjugation %)
               :tooltip {:fx/type :tooltip
                         :show-delay [100 :ms]
                         :text (str "Current lemma: " (learner/conjugation->lemma (:text-db learning-info) selected-conjugation :nilable true))}
               :text selected-conjugation
               :h-box/hgrow :always}
              {:fx/type :label
               :text " Maps to lemma: "}
              {:fx/type :text-field
               :on-text-changed #(swap! *state assoc :selected-lemmatization %)
               :tooltip {:fx/type :tooltip
                         :show-delay [10 :ms]
                         :text (str "Conjugations: " (->> learning-info :text-db :lemma->conjugations
                                                          (#(get % selected-lemmatization)) (clojure.string/join ", ")))}
               :text selected-lemmatization
               :h-box/hgrow :always}
              {:fx/type :button
               :on-action (fn [_] (do (swap! *state assoc :learning-info
                                             (if (not (or (= selected-lemmatization "") (= selected-conjugation "")))
                                               (do (println "Changing conjugation " selected-conjugation "->" selected-lemmatization)
                                                   (learner/update-lemmatization-and-reload learning-info (clojure.string/lower-case selected-conjugation) (clojure.string/lower-case selected-lemmatization)))
                                               learning-info))
                                      (println selected-conjugation "->" selected-lemmatization)))
               :text "Change lemmatization"
               :h-box/hgrow :always}]})

(defn- gui-sentence-lists [learning-info selected-learnable-sentence]
  {:fx/type :h-box
   :v-box/vgrow :always
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
               :h-box/hgrow :always}]})

(defn- gui-save-path-selector [save-path learning-info]
  {:fx/type :h-box
   :children [{:fx/type :label
               :text "Save file path: "}
              {:fx/type :text-field
               :on-text-changed #(swap! *state assoc :save-path %)
               :text save-path
               :h-box/hgrow :always}
              {:fx/type :button
               :disable (nil? learning-info)
               :on-action (fn [_] (swap! *state assoc :learning-info (learner/load-learning-progress learning-info (get-path-last-saved-learning-progress))))
               :text "Load progess"}
              {:fx/type :button
               :disable (nil? learning-info)
               :on-action (fn [_] (learner/save-learning-progress learning-info))
               :text "Save progress"}]})

(defn- gui-button-list [learning-info selected-learnable-sentence]
  {:fx/type :h-box
   :children [{:fx/type :button
               :on-action (learn-selected-sentence learning-info selected-learnable-sentence)
               :disable (nil? learning-info)
               :text "Learn selected sentence"
               :h-box/hgrow :always}]})

(defn- gui-text-path-selector [texts-path learning-info]
  {:fx/type :h-box
   :children [{:fx/type :label
               :text "Path to texts:"}
              {:fx/type :text-field
               :on-text-changed #(swap! *state assoc :texts-path %)
               :text texts-path
               :h-box/hgrow :always}
              {:fx/type :button
               :disable (some? learning-info)
               :on-action (fn [_] (swap! *state assoc :learning-info (->> texts-path
                                                                          (directory->text-db config)
                                                                          (learner/text-db->new-learn-info config))))
               :text "Load texts"}]})

(defn root-window [{:keys [title learning-info selected-learnable-sentence save-path texts-path selected-lemmatization selected-conjugation]}]
  {:fx/type :stage
   :showing true
   :title   title
   :scene   {:fx/type :scene
             :root    {:fx/type  :v-box
                       :children [(gui-text-path-selector texts-path learning-info)
                                  (gui-save-path-selector save-path learning-info)
                                  (gui-button-list learning-info selected-learnable-sentence)
                                  (gui-sentence-lists learning-info selected-learnable-sentence)
                                  (gui-conjugation-lists learning-info selected-conjugation selected-lemmatization)
                                  {:fx/type  :border-pane
                                   :left {:fx/type :label
                                          :text (let [current-lemma (learner/conjugation->lemma (:text-db learning-info) selected-conjugation :nilable true)]
                                                  (if (some? current-lemma)
                                                    (str "Current lemma: " current-lemma ".")
                                                    "No paired lemma."))}
                                   :center {:fx/type :label
                                            :text (let [conjugations (->> learning-info :text-db :lemma->conjugations
                                                                          (#(get % selected-lemmatization)) (clojure.string/join ", "))]
                                                    (if (not= "" conjugations)
                                                      (str "Conjugations: " conjugations ".")
                                                      "No conjugations."))}}]}}})

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root-window)))

(fx/mount-renderer *state renderer)
