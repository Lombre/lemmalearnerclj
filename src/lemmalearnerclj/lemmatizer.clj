(ns lemmalearnerclj.lemmatizer
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :refer :all]
   [clojure.set :as set]
   [clojure.string :as str]
   [jsonista.core :as jsonista]
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [lemmalearnerclj.textdatastructures]
   [parallel.core :as p])
  )

(defrecord Lemmatizer [language conjugation->lemma conjugation->lemmas lemma->conjugations])

(defn get-json-lines [raw-dict]
  (->> raw-dict str/lower-case str/split-lines (pmap json/read-json)))

(defn path->json-lines [path]
  (get-json-lines (slurp path)))

(defn json-to-lemma [jsonobj]
  (:word jsonobj))

(defn json-to-conjugations [jsonobj]
  (->> jsonobj
       :forms
       (filter #(contains? % :tags))
       (filter #(not (.contains (:tags %) "auxiliary")))
       (map :form)
       (filter #(not (.contains % " ")))
       (filter #(not (contains? #{"", "-"} %) ))
       set))

(defn jsonobj->lemmamap [jsonobj]
  (let [lemma (json-to-lemma jsonobj)
        conjugations (json-to-conjugations jsonobj)]
    [lemma (conj conjugations lemma)]))

(defn merge-lemma-maps [lemma-maps]
  (reducers/reduce #(merge-with into %1 %2) {} lemma-maps))

(defn json-lines->lemma->conjugation [json-lines]
  (->> json-lines
       (map jsonobj->lemmamap)
       (filter #(not (.contains (first %) " ")))
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

(defn language->save-path [language]
  (str "dictionary-files/noninflected-words-" language ".json"))

(defn language->alternative-save-path [language]
  (str "dictionary-files/" language "-saved.json"))

(defn language->personal-save-path [language]
  (str "dictionary-files/" language "-personal.json"))

(defn save-lemma->conjugations [language lemma->conjugations]
  (->> lemma->conjugations
       (into (sorted-map))
       (#(json/write-str % :escape-unicode false))
       (#(str/replace % #"]," "],\n "))
       #_(#(with-out-str (json/pprint % :escape-unicode false)))
       (spit (language->alternative-save-path language))
       ))

(defn uniformize-lemma->conjugations [conjugation->lemma lemma->conjugations]
    (into {} (for [[k v] lemma->conjugations]
               [k (->> v (filter #(= k (get conjugation->lemma %)) ) set)]))) ;; Ensure that conjugations and lemmas point at the same thing.

(defn load-saved-lemma-to-words-file [language]
  (->> language
       language->alternative-save-path
       slurp
       jsonista/read-value
       (#(p/update-vals % (fn [x] (set x))))))

(defn load-personal-dictionary [language] ; Maps from conjugation to lemma
  (if (not (.exists (io/file (language->personal-save-path language)))) {}
      (->> (language->personal-save-path language)
           slurp
           jsonista/read-value)))

(defn save-personal-dictionary [language conjugation->lemma]
  (->> conjugation->lemma
       (into (sorted-map))
       (#(json/write-str % :escape-unicode false))
       (#(str/replace % #"]," "],\n "))
       (spit (language->personal-save-path language))))

(defn update-lemmatizer-with-personal-dictionary [[[conjugation lemma] & T] lemmatizer]
  ;; (println [conjugation lemma])
  (if (nil? conjugation) lemmatizer
      (let [old-lemma (get (:conjugation->lemma lemmatizer) conjugation)
            lemma->conjugations-old-removed (->> (:lemma->conjugations lemmatizer)
                                                 (#(update % old-lemma (fn [x] (disj x conjugation lemma)))))
            updated-conjugation->lemma (->> (:conjugation->lemma lemmatizer)
                                            (#(assoc % conjugation lemma
                                                     lemma lemma))) ; If a conjugation point at a lemma, the lemma should also point to itself

            updated-lemma->conjugations (merge-with set/union lemma->conjugations-old-removed {lemma (set [conjugation lemma])})]
        (recur T (assoc lemmatizer :conjugation->lemma updated-conjugation->lemma :lemma->conjugations updated-lemma->conjugations)))))

(defn lemma->conjugations-to-lemmatizer [language lemma->conjugations]
  (let [conjugation->lemmas (invert-many-to-many lemma->conjugations)
        conjugation->lemma (choose-single-lemma conjugation->lemmas)
        lemma->conjugations-uniform (uniformize-lemma->conjugations conjugation->lemma lemma->conjugations)
        lemmatizer (Lemmatizer. language conjugation->lemma conjugation->lemmas lemma->conjugations-uniform)
        personlized-lemmatizer (update-lemmatizer-with-personal-dictionary (load-personal-dictionary language) lemmatizer)]
    personlized-lemmatizer))

(defn json-lines->lemmatizer [language json-lines & {:keys [save-lemmatizer] :or {save-lemmatizer true}}]
  (let [lemmatizer  (lemma->conjugations-to-lemmatizer language (json-lines->lemma->conjugation json-lines))
        personalized-lemmatizer (update-lemmatizer-with-personal-dictionary (load-personal-dictionary language) lemmatizer)]
    (do (if save-lemmatizer (save-lemma->conjugations language (:lemma->conjugations lemmatizer)) nil)
        personalized-lemmatizer)))

(defn language->lemmatizer [language]
  (wrap-with-print (str "Loading dictionary for language: " language)
                   (if (.exists (io/file (language->alternative-save-path language)))
                     (do (println "Loading existing lemmafile")
                         (lemma->conjugations-to-lemmatizer language (load-saved-lemma-to-words-file language)))
                     (do (println "Loading new lemmafile")
                         (json-lines->lemmatizer language (path->json-lines (language->save-path language)))))
                   (str "Finished loading dictionary")))
