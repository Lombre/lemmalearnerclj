(ns lemmalearnerclj.lemmatizer
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :refer :all]
   [clojure.string :as str :refer [includes?]]
   [jsonista.core :as jsonista]
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p])
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
       (filter #(not (.contains % " ")))
       (filter #(not (contains? #{"", "-"} %) ))
       (map #(Conjugation. %))
       set))

(defn jsonobj->lemmamap [jsonobj]
  (let [lemma (json-to-lemma jsonobj)
        conjugations (json-to-conjugations jsonobj)]
    [lemma (conj conjugations (Conjugation. (:raw lemma)))]))

(defn merge-lemma-maps [lemma-maps]
  (reducers/reduce #(merge-with into %1 %2) {} lemma-maps))

(defn json-lines->lemma->conjugation [json-lines]
  (->> json-lines
       (map jsonobj->lemmamap)
       (filter #(not (.contains (:raw (first %)) " ")))
       (map #(identity {(first %) (second %)}))
       merge-lemma-maps))

(defn invert-many-to-many
  "returns a many-to-many mapping"
  ([m] (invert-many-to-many #{} m))
  ([to m]
   (persistent!
    (reduce (fn [m [k vs]]
              (reduce (fn [m v] (assoc! m v (conj (get m v to) k))) m vs))
            (transient {}) m))))

(defn choose-single-lemma [word->lemmas]
;;;  Choose the word itself, if it is an option, otherwise the first option.
  (->> word->lemmas
       (#(into {}
               (for [[k v] %]
                 [k (if (contains? v k) k (first v))])))))


(vector {"test" ["fisk", "kage"]})

(defn language->save-path [language]
  (str "dictionary-files/noninflected-words-" language ".json"))

(defn language->alternative-save-path [language]
  (str "dictionary-files/" language "-saved.json"))

(defn save-lemma->conjugations [language lemma->conjugations]
  (->> lemma->conjugations
       (#(update-keys % :raw))
       (#(update-vals % (partial map :raw)))
       (into (sorted-map))
       (#(json/write-str % :escape-unicode false))
       (#(str/replace % #"]," "],\n "))
       #_(#(with-out-str (json/pprint % :escape-unicode false)))
       (spit (language->alternative-save-path language))
       ))

(defn uniformize-lemma->conjugations [conjugation->lemma lemma->conjugations]
    (into {} (for [[k v] lemma->conjugations]
               [k (->> v (filter #(= k (get conjugation->lemma %)) ) set)]))) ;; Ensure that conjugations and lemmas point at the same thing.
  ;; (let [raw-lemmas (set (map :raw (keys lemma->conjugations)))]
  ;;   (into {} (for [[k v] lemma->conjugations]
  ;;              [k (->> v (filter #(or (not (contains? raw-lemmas (:raw %)))
  ;;                                     (= (:raw k) (:raw %)))))])))

(defn lemma->conjugations-to-lemmatizer [language lemma->conjugations]
  (let [conjugation->lemmas (invert-many-to-many lemma->conjugations)
        conjugation->lemma (choose-single-lemma conjugation->lemmas)
        lemma->conjugations-uniform (uniformize-lemma->conjugations conjugation->lemma lemma->conjugations)]
    (Lemmatizer. language conjugation->lemma conjugation->lemmas lemma->conjugations-uniform)))

(defn json-lines->lemmatizer [language json-lines & {:keys [save-lemmatizer] :or {save-lemmatizer true}}]
  (let [lemmatizer  (lemma->conjugations-to-lemmatizer language (json-lines->lemma->conjugation json-lines))]
    (do (if save-lemmatizer (save-lemma->conjugations language (:lemma->conjugations lemmatizer)) nil)
        lemmatizer)))

(defn load-saved-lemma-to-words-file [language]
  (->> language
       language->alternative-save-path
       slurp
       jsonista/read-value
       (#(update-keys % (fn [x] (Lemma. (name x))))) ; The keys and values are strings, so we need to map back
       (#(p/update-vals % (fn [x] (set (map (fn [y] (Conjugation. y)) x)))))))

(defn language->lemmatizer [language]
  (wrap-with-print
   (str "Loading dictionary for language: " language)
   (if (.exists (io/file (language->alternative-save-path language)))
     (do (println "Loading existing lemmafile")
         (lemma->conjugations-to-lemmatizer language (load-saved-lemma-to-words-file language)))
     (do (println "Loading new lemmafile")
         (json-lines->lemmatizer language (path->json-lines (language->save-path language)))))
   (str "Finished loading dictionary")))
