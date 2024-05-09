(ns lemmalearnerclj.parser ;  (:require [testproject.textdatabase])
  (:require [lemmalearnerclj.textdatastructures])
  (:import [lemmalearnerclj.textdatastructures Text Paragraph Sentence Conjugation]))

;; (require '[clojure.data.priority-map :refer [priority-map]])
(require '[clojure.core.match :refer [match]])
(require '[clojure.string :as str])
(require '[clojure.set])
(require '[clojure.java.io :as io])
;; (require '[clojure.core.reducers :as reducers])

(def punctuation (set '(\. \! \?)))
(def quote-starters {\" \"
                     \“ \”
                     \' \'
                     \( \)
                     \[ \]})
(def other-punctuation #{\, \; \:})
(def all-quote-chars (set (concat (keys quote-starters) (vals quote-starters))))

(defn punctuation? [char] (contains? punctuation char))
(defn quote-starter? [char] (contains? quote-starters char))
(defn single-quote-starter? [char] (contains? #{\'} char))

(defn at-start-of-quote? [cur-char next-char]
  (and (quote-starter? cur-char) (or (not (single-quote-starter? cur-char)) (contains? #{\space nil} next-char))))

(defn conj-if [condition collection element]
  (if condition
    (conj collection element)
    collection))

(defn paragraph->words [paragraph]
  (->> (:sentences paragraph) (map :words) (apply clojure.set/union)))

(defn word-seperator? [char]
  (contains? #{\space \- \— \’ \'} char))

(defn at-end-of-word? [cur-char]
  (or (word-seperator? cur-char) (contains? all-quote-chars cur-char) (contains? other-punctuation cur-char)))

(defn at-end-of-sentence? [cur-char next-char]
  (or (and (punctuation? cur-char) (or (= \space next-char) (= nil next-char))) (= nil cur-char)))

(defn text-str [input]
  (cond (instance? Paragraph input) (:raw input)
        :else input))

(defn parse-raw-conjugation
  [raw-word] (Conjugation. (str/lower-case (apply str  raw-word))))

(defn parse-raw-sentence
  ([raw-sentence]
   (let [parsed-sentence (parse-raw-sentence raw-sentence [] #{} [])]
     (assoc parsed-sentence :raw (str/trim (apply str (map text-str raw-sentence))))))
  ([remaining-raw-sentence sub-paragraphs words cur-word]
   (let [cur-char (first remaining-raw-sentence)]
     (cond (at-end-of-word? cur-char)
           #_=> (let [updated-words (conj-if (and (not (nil? cur-word)) (not (empty? cur-word))) words (parse-raw-conjugation cur-word))]
                  (recur (rest remaining-raw-sentence) sub-paragraphs updated-words []))

           (instance? Paragraph cur-char) ;; It is a sub paragraph
           #_=> (recur (rest remaining-raw-sentence) (conj sub-paragraphs cur-char) (clojure.set/union words (paragraph->words cur-char)) cur-word)

           (at-end-of-sentence? cur-char (second remaining-raw-sentence))
           #_=> (Sentence. "" sub-paragraphs (conj-if (and (not (nil? cur-word)) (not (empty? cur-word))) words (parse-raw-conjugation cur-word)))

           :else ;; Going throug a word
           #_=> (recur (rest remaining-raw-sentence) sub-paragraphs words (conj cur-word cur-char))))))

(defn find-end-of-quote
  ; Finds end of quoute, starting from a given quote
  ; Returns [quouted-part, remaining-part-of-paragraph]
  ([paragraph quote-starter quote-ender] (find-end-of-quote paragraph quote-starter quote-ender [] 0))
  ([paragraph quote-starter quote-ender quoted-part quote-starter-count]
   (let [cur-char  (first paragraph)]
     (match [cur-char quote-starter-count]
            [quote-ender 0] [(reverse quoted-part) (rest paragraph)] ; Done!
            [quote-ender _] (recur (rest paragraph) quote-starter quote-ender (cons cur-char quoted-part) (- 1 quote-starter-count))
            [quote-starter _] (recur (rest paragraph) quote-starter quote-ender (cons cur-char quoted-part) (+ 1 quote-starter-count))
            [nil _] '(nil) ; Error case
            :else (recur (rest paragraph) quote-starter quote-ender (cons cur-char quoted-part) quote-starter-count)))))

(defn parse-raw-paragraph
  ([raw-paragraph]
    (let [parsed-paragraph (parse-raw-paragraph raw-paragraph [] [])] ;; Add raw paragraph to the paragraph
        (assoc parsed-paragraph :raw (apply str (map text-str raw-paragraph)))))
  ([remaining-raw-paragraph sentences current-sentence]
   (let [cur-char  (first remaining-raw-paragraph)
         next-char (second remaining-raw-paragraph)]
     (cond (nil? cur-char)
           #_=> (let [paragraph-sentences (conj-if (not (empty? current-sentence)) sentences (parse-raw-sentence (reverse current-sentence)))]
                  (Paragraph. "" paragraph-sentences))
           (at-end-of-sentence? cur-char next-char)
           #_=> (let [new-sentence (parse-raw-sentence (reverse (cons cur-char current-sentence)))]
                  (recur (rest remaining-raw-paragraph) (conj sentences new-sentence) []))
           (at-start-of-quote? cur-char next-char) ;; Parse until the end of the quote
           #_=> (let [[quoted-part paragraph-after-quoted-part] (find-end-of-quote (rest remaining-raw-paragraph) cur-char (get quote-starters cur-char))
                      parsed-sub-paragraph (parse-raw-paragraph quoted-part)
                      current-sentence-with-subsentence (concat [(get quote-starters cur-char)] [parsed-sub-paragraph] [cur-char] current-sentence)]
                  ;; Continue from there
                  (if (nil? parsed-sub-paragraph)
                    nil ;; Sub paragraph could not be parsed properly
                    (parse-raw-paragraph paragraph-after-quoted-part sentences current-sentence-with-subsentence)))
           :else ;; Normal char
           #_=> (recur (rest remaining-raw-paragraph) sentences (cons cur-char current-sentence))))))

(defn read-text-from-path [path]
  (slurp path))

(defn split-into-raw-paragraphs [text]
  (->> text ((fn [x] (str/split x #"\n")))))

(defn parse-raw-text [text-name raw-text]
  (->> raw-text
       split-into-raw-paragraphs
       (map str/trim)
       (filter #(not= "" %))
       (map seq)
       (map parse-raw-paragraph)
       (Text. (.getName (io/file text-name)))))

(defn text-path->text [text-path]
  (->> text-path
       read-text-from-path
       (parse-raw-text text-path)))
