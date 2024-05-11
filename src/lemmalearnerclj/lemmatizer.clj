(ns lemmalearnerclj.lemmatizer
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.data.json :as json]
   [clojure.pprint :refer :all]
   [clojure.string :as str]
   [lemmalearnerclj.textdatastructures])
  (:import [lemmalearnerclj.textdatastructures Conjugation Lemma])
  )

(defrecord Lemmatizer [language word->lemma word->lemmas lemma->words])

(def test-wikitionary-file-path "dictionary-files/noninflected-words-danish.json")

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
  (->> word->lemmas
       (#(into {} (for [[k v] %] [k (if (contains? v k) k
                                        (first v))])))))

(defn language->save-path [language]
  (str "dictionary-files/" language ".json"))

(defn language->alternative-save-path [language]
  (str "dictionary-files/" language "-saved.json"))

(defn save-lemma->words [language lemma->words]
  (->> lemma->words
       (#(update-keys % :raw))
       (#(update-vals % (partial map :raw)))
       (json/write-str)
       ;; json/pprint
       ;; with-out-str
       (spit (language->alternative-save-path language))))

(defn json-lines->lemmatizer [language json-lines]
  (let [lemma->words (json-lines->lemma->conjugation json-lines)
        word->lemmas (invert-many-to-many lemma->words)
        word->lemma (choose-single-lemma word->lemmas)]
    (Lemmatizer. language word->lemma word->lemmas lemma->words)))

(defn language->lemmatizer [language]
  (let [file-path (str "dictionary-files/noninflected-words-" language ".json")
        dictionary-json-lines (path->json-lines file-path)]
    (json-lines->lemmatizer language dictionary-json-lines)))
;; => #'lemmalearnerclj.lemmatizer/language->lemmatizer

(defn load-saved-lemma-to-words-file [language]
  (->> language
       language->alternative-save-path
       slurp
       json/read-json
       (#(update-keys % (fn [x] (Lemma. (name x))))) ; The keys and values are strings, so we need to map back
       (#(update-vals % (partial map (fn [x] (Conjugation. x)))))))

(def danish-lemmatizer (language->lemmatizer "danish"))

(save-lemma->words "danish" (:lemma->words danish-lemmatizer))

(= (load-saved-lemma-to-words-file "danish") (:lemma->words danish-lemmatizer))

(def danish-lines (path->json-lines (str "dictionary-files/noninflected-words-" "danish" ".json")))

(defn test-problem [[line & remainder]]
  (if (nil? line) true
      (do
        (println (:word line))
        (->> line
             json-to-conjugations
             count
             pprint)
        (recur remainder))))

;; (->> danish-lines test-problem)

(->>  danish-lines
     (filter #(= (Lemma. "øde") (json-to-lemma %)))
     (map :forms)
     ;; (:forms)
     ;; (filter #(contains? % :tags))
     ;; (filter #(not (.contains (:tags %) "auxiliary")))
     ;; (map :tags)
     ;; (filter #(.contains % "auxiliary"))
     )

;; (->> danish-lemmatizer
;;      :lemma->words
;;      (take 10)
;;      pprint
;;      ;; (#(get % (Lemma. "klima")))

;;      ;; (filter #(->> % key :raw (= "bombe")))
;;      ;; (run! println)
;;      )
;; (get (:lemma->words danish-lemmatizer) (Lemma. "bombe"))
;;
(->> danish-lemmatizer
     :word->lemmas
     (sort-by #(count (val %)) >)
     (take 50)
     (run! pprint)
     ;; val
     ;; count
     ;; pprint
     ;; pprint
     ;; spy
     ;; (#(str "dictionary-files/noninflected-words-" % ".json"))
     ;; spy-talk
     ;; path->json-lines
     ;; spy-talk
     ;; json-lines->lemma->conjugation
     ;; (invert-many-to-many)
     ;; pprint
     )
