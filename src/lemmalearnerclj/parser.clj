(ns lemmalearnerclj.parser ;  (:require [testproject.textdatabase])
  (:require
   [lemmalearnerclj.helper :refer [getx]]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures
    Conjugation
    Paragraph
    Sentence
    Text]))

(require '[clojure.core.match :refer [match]])
(require '[clojure.string :as str])
(require '[clojure.set])
(require '[clojure.java.io :as io])

(defn punctuation? [parse-conf char] (contains? (getx parse-conf :punctuation) char))
(defn quote-starter? [parse-conf char] (contains? (getx parse-conf :quote-pairs ) char))
(defn single-quote-starter? [char] (contains? #{\'} char))
(defn parse-conf->quote-symbols [parse-conf]
  (let [quote-pairs (getx parse-conf :quote-pairs)]
    (set (concat (vals quote-pairs) (keys quote-pairs)))))

(defn parse-conf->end-of-words-chars [parse-conf]
  (set (concat #{\space \- \— \’ \, \' \…}
               (getx parse-conf :punctuation)
               (parse-conf->quote-symbols parse-conf)
               (getx parse-conf :other-punctuation))))

(defn at-start-of-quote? [parse-conf cur-char next-char]
  (and (quote-starter? parse-conf cur-char) (or (not (single-quote-starter? cur-char))
                                                (contains? #{\space nil} next-char))))

(defn conj-if [condition collection element]
  (if condition
    (conj collection element)
    collection))

(defn paragraph->words [paragraph]
  (->> (:sentences paragraph) (map :words) (apply clojure.set/union)))

(defn word-seperator? [char]
  (contains? #{\space \- \— \’ \' \…} char))

(defn at-end-of-word? [parse-conf cur-char]
  (if (contains? parse-conf :end-of-word-chars)
    (get (get parse-conf :end-of-word-chars) cur-char)
    (get (parse-conf->end-of-words-chars parse-conf) cur-char)))

(defn at-end-of-sentence? [parse-conf cur-char next-char]
  (or (and (punctuation? parse-conf cur-char) (or (= \space next-char) (= nil next-char))) (= nil cur-char)))

(defn text-str [input]
  (cond (instance? Paragraph input) (:raw input)
        :else input))

(defn parse-raw-conjugation
  [raw-word] (Conjugation. (str/lower-case (apply str  raw-word))))

(defn parse-raw-sentence
  ([parsing-config raw-sentence]
   (let [parsed-sentence (parse-raw-sentence parsing-config raw-sentence [] #{} [])]
     (assoc parsed-sentence :raw (str/trim (apply str (map text-str raw-sentence))))))
  ([parsing-config [cur-char & remaining] sub-paragraphs words cur-word]
   (cond (at-end-of-word? parsing-config cur-char)
         #_=> (let [updated-words (conj-if (and (not (nil? cur-word)) (not (empty? cur-word))) words (parse-raw-conjugation cur-word))]
                (recur parsing-config remaining sub-paragraphs updated-words []))

         (instance? Paragraph cur-char) ;; It is a sub paragraph
         #_=> (recur parsing-config remaining (conj sub-paragraphs cur-char) (clojure.set/union words (paragraph->words cur-char)) cur-word)

         (at-end-of-sentence? parsing-config cur-char (first remaining))
         #_=> (Sentence. "" sub-paragraphs (conj-if (and (not (nil? cur-word)) (not (empty? cur-word))) words (parse-raw-conjugation cur-word)))

         :else ;; Going throug a word
         #_=> (recur parsing-config remaining sub-paragraphs words (conj cur-word cur-char)))))

(defn find-end-of-quote
  ; Finds end of quoute, starting from a given quote
  ; Returns [quouted-part, remaining-part-of-paragraph]
  ([paragraph quote-starter quote-ender] (find-end-of-quote paragraph quote-starter quote-ender [] 0))
  ([[cur-char & remaining] quote-starter quote-ender quoted-part quote-starter-count]
   (match [cur-char quote-starter-count]
          [quote-ender 0] [(reverse quoted-part) remaining] ; Done!
          [quote-ender _] (recur remaining quote-starter quote-ender (cons cur-char quoted-part) (- 1 quote-starter-count))
          [quote-starter _] (recur remaining quote-starter quote-ender (cons cur-char quoted-part) (+ 1 quote-starter-count))
          [nil _] '(nil)                ; Error case
          :else (recur remaining quote-starter quote-ender (cons cur-char quoted-part) quote-starter-count))))

(defn parse-raw-paragraph
  ([parsing-config raw-paragraph]
   (let [parsed-paragraph (parse-raw-paragraph parsing-config raw-paragraph [] [])] ;; Add raw paragraph to the paragraph
     (assoc parsed-paragraph :raw (apply str (map text-str raw-paragraph)))))
  ([parsing-config [cur-char next-char & _ :as remaining-raw-paragraph] sentences current-sentence]
   (cond (nil? cur-char)
         (let [paragraph-sentences (conj-if (not (empty? current-sentence))
                                            sentences (parse-raw-sentence parsing-config (reverse current-sentence)))]
           (Paragraph. "" paragraph-sentences))
         (at-end-of-sentence? parsing-config cur-char next-char)
         (let [new-sentence (parse-raw-sentence parsing-config (reverse (cons cur-char current-sentence)))]
           (recur parsing-config (rest remaining-raw-paragraph) (conj sentences new-sentence) []))
         (at-start-of-quote? parsing-config cur-char next-char) ;; Parse until the end of the quote
         (let [[quoted-part paragraph-after-quoted-part]
               (find-end-of-quote (rest remaining-raw-paragraph) cur-char (getx (:quote-pairs parsing-config) cur-char))
               parsed-sub-paragraph (parse-raw-paragraph parsing-config quoted-part)
               current-sentence-with-subsentence (concat [(getx (:quote-pairs parsing-config) cur-char)]
                                                         [parsed-sub-paragraph] [cur-char] current-sentence)]
           ;; Continue from there
           (if (nil? parsed-sub-paragraph)
             nil ;; Sub paragraph could not be parsed properly
             (parse-raw-paragraph parsing-config paragraph-after-quoted-part sentences current-sentence-with-subsentence)))
         :else ;; Normal char
         #_=> (recur parsing-config (rest remaining-raw-paragraph) sentences (cons cur-char current-sentence)))))

(defn read-text-from-path [path]
  (slurp path))

(defn split-into-raw-paragraphs [text]
  (->> text ((fn [x] (str/split x #"\n")))))

(defn parse-raw-text [parsing-config text-name raw-text]
  (->> raw-text
       split-into-raw-paragraphs
       (pmap str/trim)
       (filter #(not= "" %))
       (pmap #(->> %
                   seq
                   (parse-raw-paragraph parsing-config)))
       doall
       (Text. (.getName (io/file text-name)))))

(defn text-path->text [parsing-config text-path]
  (let [updated-parsing-config (assoc parsing-config :end-of-word-chars (parse-conf->end-of-words-chars parsing-config))
        text (->> text-path read-text-from-path (parse-raw-text updated-parsing-config text-path))]
    (println (str "Parsed " (.getName (io/file text-path))))
    text))
