(ns lemmalearnerclj.lemmatizer
  (:require
   [clj-async-profiler.core :as prof]
   [clojure.core.reducers :as reducers]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :refer :all]
   [clojure.string :as str]
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p]
   [jsonista.core :as jsonista])
  (:import
   [lemmalearnerclj.textdatastructures Conjugation Lemma]))

(defrecord Lemmatizer [language conjugation->lemma conjugation->lemmas lemma->conjugations])

(defn get-json-lines [raw-dict]
  (->> raw-dict str/lower-case str/split-lines (pmap json/read-json)))

(defn path->json-lines [path]
  (get-json-lines (slurp path)))

(defn json-to-lemma [jsonobj]
  (Lemma. (:word jsonobj)))

(defn json-to-conjugations [jsonobj]
  (->> jsonobj
       :forms
       (filter #(contains? % :tags))
       (filter #(not (.contains (:tags %) "auxiliary")))
       (map :form)
       (filter #(not (contains? #{"", "-"} %) ))
       (map #(Conjugation. %))
       set))

(defn jsonobj->lemmamap [jsonobj]
  (let [lemma (json-to-lemma jsonobj)
        conjugations (json-to-conjugations jsonobj)]
    {lemma (conj conjugations (Conjugation. (:raw lemma)))}))

(defn merge-lemma-maps [lemma-maps]
  (reducers/reduce #(merge-with into %1 %2) {} lemma-maps))

(defn json-lines->lemma->conjugation [json-lines]
  (->> json-lines (pmap jsonobj->lemmamap) merge-lemma-maps))

(defn invert-many-to-many
  "returns a many-to-many mapping"
  ([m] (invert-many-to-many #{} m))
  ([to m]
   (persistent!
    (reduce (fn [m [k vs]]
              (reduce (fn [m v] (assoc! m v (conj (get m v to) k))) m vs))
            (transient {}) m))))

(defn choose-single-lemma [word->lemmas]
  (->> word->lemmas                     ; Choose the word itself, if it is an option, otherwise the first option.
       (#(into {} (for [[k v] %] [k (if (contains? v k) k
                                        (first v))])))))

(defn language->save-path [language]
  (str "dictionary-files/noninflected-words-" language ".json"))

(defn language->alternative-save-path [language]
  (str "dictionary-files/" language "-saved.json"))

(defn save-lemma->words [language lemma->words]
  (->> lemma->words
       (#(update-keys % :raw))
       (#(update-vals % (partial map :raw)))
       (json/write-str)
       (spit (language->alternative-save-path language))))

(defn lemma->words-to-lemmatizer [language lemma->conjugations]
  (let [conjugation->lemmas (invert-many-to-many lemma->conjugations)
        conjugation->lemma (choose-single-lemma conjugation->lemmas)]
    (Lemmatizer. language conjugation->lemma conjugation->lemmas lemma->conjugations)))

(defn json-lines->lemmatizer [language json-lines & {:keys [save-lemmatizer] :or {save-lemmatizer true}}]
  (let [lemmatizer  (lemma->words-to-lemmatizer language (json-lines->lemma->conjugation json-lines))]
    (do (if save-lemmatizer (save-lemma->words language (:lemma->words lemmatizer)) nil)
        lemmatizer)))

(defn load-saved-lemma-to-words-file [language]
  (->> language
       language->alternative-save-path
       slurp
       jsonista/read-value
       (#(update-keys % (fn [x] (Lemma. (name x))))) ; The keys and values are strings, so we need to map back
       (#(p/update-vals % (fn [x] (set (map (fn [y] (Conjugation. y)) x)))))
       ))

(defn language->lemmatizer [language]
  (wrap-with-print (str "Loading dictionary for language:" language)
      (if (.exists (io/file (language->alternative-save-path language)))
        (do (println "Loading existing lemmafile")
            (lemma->words-to-lemmatizer language (load-saved-lemma-to-words-file language)))
        (do (println "Loading new lemmafile")
            (json-lines->lemmatizer language (path->json-lines (language->save-path language)))))
      (str "Finished loading dictionary")))

;; (prof/profile
;;     (->> (language->lemmatizer "english")
;;          :lemma->words
;;          (take 10)
;;          pprint))
;; (prof/serve-ui 8080)
