(ns lemmalearnerclj.textdatabase
                                        ; (:require [testproject.textdatastructures])
                                        ; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
  (:require
   [lemmalearnerclj.lemmatizer :as lemmatizer]
   [lemmalearnerclj.parser :as parser]
   [lemmalearnerclj.textdatastructures])
  (:import [lemmalearnerclj.textdatastructures Lemma]))

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
      (map :sentences)
      (flatten)))

(defn directory->file-paths [directory-path]
  (map #(.getAbsolutePath %) (.listFiles (clojure.java.io/file directory-path))))

(defn parse-texts-in-directory [directory]
  (->> directory
       (directory->file-paths)
       (pmap #(->> %
                   parser/text-path->text))))

(defn texts->sentences [texts]
  (->> texts
       (map text->sentences)
       flatten))

(defn sentences->words [sentences]
  (->> sentences
       (map #(vec (:words %)))
       (flatten)
       (set)))

(defn sentences->word->sentences [sentences]
  (->> sentences
       (map #(->> % ;; get hashmap raw-word -> [sentence]
                  :words
                  (reducers/reduce (fn [xs x] (assoc xs x #{%})) {})))
       (reducers/fold (partial merge-with into))))

(defn conjugations->lemmas [conjugation->lemma conjugations]
  (->> conjugations
       (map #(get conjugation->lemma % ))
       (filter some?)
       set))


(defn texts->text-database [{language :language} texts]
  (let [{lemma->conjugations :lemma->conjugations conjugation->lemma :conjugation->lemma} (lemmatizer/language->lemmatizer language)
        sentences (texts->sentences texts)
        conjugations (sentences->words sentences)
        lemmas (conjugations->lemmas conjugation->lemma conjugations)
        word->sentences (sentences->word->sentences sentences)]
    (Textdatabase. texts sentences conjugations lemmas word->sentences
                   lemma->conjugations conjugation->lemma)))

(defn directory->text-database [config directory]
  (println "Parsing texts")
  (let [texts (parse-texts-in-directory directory)]
    (println "Finished parsing texts")
    (texts->text-database config texts)))
