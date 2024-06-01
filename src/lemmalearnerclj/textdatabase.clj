(ns lemmalearnerclj.textdatabase
                                        ; (:require [testproject.textdatastructures])
                                        ; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [lemmalearnerclj.helper :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatastructures]))

(require '[clojure.core.reducers :as reducers])

(def textfiles-directory "test-texts/")

(defrecord Textdatabase [texts
                         sentences
                         conjugations
                         lemmas
                         conjugation->sentences
                         lemma->conjugations
                         conjugation->lemma])

(defn text->sentences [text]
  (->> text
       (:paragraphs)
       (mapcat :sentences)
       (distinct)))

(defn directory->file-paths [directory-path]
  (map #(.getAbsolutePath %) (.listFiles (clojure.java.io/file directory-path))))

(defn parse-texts-in-directory [directory]
  (->> directory
       (directory->file-paths)
       (pmap #(parser/text-path->text %))))

(defn texts->sentences [texts]
  (->> texts
       (mapcat text->sentences)))

(defn sentences->words [sentences]
  (->> sentences
       (mapcat :words)
       set
       doall))

(defn sentences->word->sentences [sentences]
  (->> sentences
       (pmap #(->> % ;; get hashmap raw-word -> [sentence]
                   :words
                   (reducers/reduce (fn [xs x] (assoc xs x [%])) {})))
       ;; (merge-with into)
       (helper/preduce (partial merge-with into))
       (doall)))

(defn conjugations->lemmas [conjugation->lemma conjugations]
  (->> conjugations
       (map #(get conjugation->lemma % ))
       (filter some?)
       distinct
       doall))

(defn sentence->sentence-with-lemmas [conjugation->lemma sentence]
  (let [lemmas (->> sentence :words (map #(get conjugation->lemma %)) (filter some?))]
    (assoc sentence :lemmas lemmas)))

(defn sentences->sentences-with-lemmas [conjugation->lemma sentences]
  (->> sentences
       (pmap #(sentence->sentence-with-lemmas conjugation->lemma %))
       doall))

(defn filter-sentences-for-learning [sentences]
  (->> sentences
       (filter #(->> % :raw count (>= 70)))))

(defn texts->text-database [{language :language} texts]
  (println "Converting to text database.")
  (let [{lemma->conjugations :lemma->conjugations conjugation->lemma :conjugation->lemma} (lemmatizer/language->lemmatizer language)
        sentences-with-lemmas (sentences->sentences-with-lemmas conjugation->lemma (texts->sentences texts))
        filtered-sentences (filter-sentences-for-learning sentences-with-lemmas)
        _ (println "Before filtering: " (count sentences-with-lemmas) ", after filtering: " (count filtered-sentences))
        conjugations (sentences->words filtered-sentences)
        lemmas (conjugations->lemmas conjugation->lemma conjugations)
        word->sentences (sentences->word->sentences filtered-sentences)]
    (println "Finished converting.")
    (Textdatabase. texts filtered-sentences conjugations lemmas word->sentences
                   lemma->conjugations conjugation->lemma)))

(defn directory->text-database [config directory]
  (println "Parsing texts")
  (let [texts (doall (parse-texts-in-directory directory))]
    (println "Finished parsing texts")
    (texts->text-database config texts)))
