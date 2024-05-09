(ns lemmalearnerclj.textdatabase
; (:require [testproject.textdatastructures])
; (:import [testproject.textdatastructures Text Paragraph Sentence Conjugation Lemma])
 (:require [lemmalearnerclj.parser :as parser]))

(require '[clojure.core.reducers :as reducers])

(def textfiles-directory "test-texts/")

(defrecord Textdatabase [texts
                         sentences
                         words
                         word->sentences])

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

(defn texts->text-database [texts]
  (let [sentences (texts->sentences texts)
        words (sentences->words sentences)
        word->sentences (sentences->word->sentences sentences)]
    (Textdatabase. texts sentences words word->sentences)))

(defn directory->text-database [directory]
  (println "Parsing texts")
  (let [texts (parse-texts-in-directory directory)]
    (println "Finished parsing texts")
    (texts->text-database texts)))
