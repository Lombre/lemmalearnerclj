(ns lemmalearnerclj.testparser
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [lemmalearnerclj.textdatastructures :as textdatastructures]))

(def config
  {:language "english"
   :should-print? true
   :parsing-config {:punctuation #{\. \! \?}
                    :quote-pairs {\" \"
                                  \“ \“
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

;; (set! *warn-on-reflection* true)
(def error-sentence " “Never is a very long time, Ms. Murphy.” Marcone blinked slowly and then sighed. “Clearly, the atmosphere has become unproductive,” he said. “Ms. Gard, please escort them both from the premises. Give them the information they want.“")
(map int [\“ \“])
(def test-sentence ".This is( a test) 99 sentence!")
(def parse-word #"\w+")
(def parse-word-and-punct #"\w+|\p{P}")   ;"\w+|\W+|\p{Punct}"

(defn vec-re-seq
  "Something like re-seq"
  {:static true}
  [^java.util.regex.Pattern re ^String s]
  (let [matcher (re-matcher re s)]
    (loop [^String match (re-find matcher) ;loop starts with 2 set arguments
           result (transient [])]
      (if-not match
        (persistent! result)
        (recur (re-find matcher)        ;loop with 2 new arguments
               (conj! result match))))))

(defn raw-text->tokens [^String raw-paragraph]
  (->> raw-paragraph (vec-re-seq parse-word-and-punct)))

(defn matches-quote-start? [^String token]
  (contains? (->> config :parsing-config :quote-pairs) (.charAt token 0)))

(defn get-matching-quote [^String token]
  (get (->> config :parsing-config :quote-pairs) (.charAt token 0)))


(defn tokens->raw-sentence
  ([tokens] (str/join (tokens->raw-sentence [(first tokens)] 1 tokens)))
  ([current-raw ^java.lang.Number pos ^clojure.lang.PersistentVector tokens]
   (if (< pos (.size tokens))
     (let [token (nth tokens pos)
           last-token (nth tokens (dec pos))]
       (do (println token)
           (cond
             (record? token) (recur (conj current-raw (:raw token)) (inc pos) tokens)
             (re-matches parse-word token) (recur (conj current-raw  (if (matches-quote-start? last-token) "" " ") token) (inc pos) tokens)
             (matches-quote-start? token) (recur (conj current-raw " " token) (inc pos) tokens)
             :else (recur (conj current-raw token) (inc pos) tokens))))
     current-raw)))

(defn tokens->sentence [tokens]
  (let [unpacked-words (mapcat #(if (record? %) (:words %) [%] ) tokens)]
    (textdatastructures/->Sentence (tokens->raw-sentence tokens) [] (vec (filter #(re-matches parse-word %) unpacked-words)))))

(->> test-sentence raw-text->tokens tokens->sentence)

(defn find-position-of-matching-quote [^java.lang.Number pos ^java.lang.Number end-pos ^Character end-quote ^clojure.lang.PersistentVector tokens]
  (if (>= pos end-pos) nil
      (let [token ^String (nth tokens pos)]
        (cond
          (= (.charAt token 0) end-quote) pos
          ;; nested quotes
          (matches-quote-start? token) (when-let [nested-end-pos (find-position-of-matching-quote (inc pos) end-pos (get-matching-quote token) tokens)]
                                         (recur (inc nested-end-pos) end-pos end-quote tokens))
          ;; continue
          :else (recur (inc pos) end-pos end-quote tokens)))))

(defn parse-token-seq
  ([tokens] (let [result (parse-token-seq [] 0 (count tokens) tokens)]
              result))
  ([current-sentence ^java.lang.Number pos ^java.lang.Number end-pos ^clojure.lang.PersistentVector tokens]
   (if (<= end-pos pos) (if (empty? current-sentence) nil (tokens->sentence current-sentence))
       (let [token ^String (nth tokens pos)]
         (cond (and (= (.length token) 1) (matches-quote-start? token)) ; Start of quote
               (when-let [end-quote-pos (find-position-of-matching-quote (inc pos) end-pos (get-matching-quote token) tokens)]
                 (recur (conj current-sentence
                              (nth tokens pos)
                              (parse-token-seq (subvec tokens (inc pos) end-quote-pos))
                              (nth tokens end-quote-pos)) (inc end-quote-pos) end-pos tokens))
               (contains? (->> config :parsing-config :punctuation) (.charAt ^String token 0)) (cons (tokens->sentence (conj current-sentence token)) (parse-token-seq [] (inc pos) end-pos tokens) )
               :else (recur (conj current-sentence token) (inc pos) end-pos tokens))))))

(->> "sentence. Sentence." (str/join) raw-text->tokens parse-token-seq pprint/pprint)

;; (println "Starting server")

;; (prof/serve-ui 8080)


(defn parse-text [path]
  (->> (slurp path)
       (^String str/split-lines)
       (map str/trim)
       (filter #(not (str/blank? %)))
       (map raw-text->tokens)
       (filter #(and (some? %) (< (.size ^clojure.lang.PersistentVector %) 5000) ))
                                     ; To avoid stack overflow.
       (mapv parse-token-seq)
       doall))

(->> (raw-text->tokens "kage er godt") class)

(defn directory->file-paths [directory-path]
  (->> (.listFiles (clojure.java.io/file directory-path))
       (filter #(str/ends-with? % ".txt"))
       (map #(.getAbsolutePath %))))

;; (prof/profile (time (->> "/home/jesper/OneDrive/projects/lemmalearnerclj/texts/english/"
;;                          (textdatabase/parse-texts-in-directory (:parsing-config config))
;;                          ;; (pmap #(parse-text %))
;;                          (doall))))
(time (->> "/home/jesper/OneDrive/projects/lemmalearnerclj/texts/english/"
           (directory->file-paths)
           (pmap #(parse-text %))
           (doall)))

(class (raw-text->tokens "This is a test"))
(println "Done")
