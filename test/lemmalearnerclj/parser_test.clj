(ns lemmalearnerclj.parser-test
  (:require [lemmalearnerclj.parser :refer :all]
            [lemmalearnerclj.textdatastructures :refer :all]
            [clojure.test :refer :all])
  (:import [lemmalearnerclj.textdatastructures Text Paragraph Sentence Conjugation]))

(deftest parse-simple-word
  (testing "Singleton word parsed incorrectly"
    (let [input-word "test"
          output-sentence (parse-raw-sentence input-word)]
      (is (= (->Sentence input-word [] #{(->Conjugation input-word)}) output-sentence)))))

(deftest parse-simple-word-lowercasing
  (testing "Singleton word parsed incorrectly. It should be lowercased"
    (let [input-word "TeSTinG"
          output-sentence (parse-raw-sentence input-word)]
      (is (= (->Sentence input-word [] #{(->Conjugation (.toLowerCase input-word))}) output-sentence)))))

(deftest parse-simple-sentence
  (testing "Simple sentence parsed incorrectly"
    (let [input-sentence "tests are good."
          output-sentence (parse-raw-sentence input-sentence)]
      (is (= (->Sentence input-sentence [] #{(->Conjugation "tests") (->Conjugation "are") (->Conjugation "good")}) output-sentence)))))

(deftest parse-sentence-with-duplicate-words
  (testing "Duplicate words handeled incorrectly incorrectly"
    (let [input-sentence "tests tests are good."
          output-sentence (parse-raw-sentence input-sentence)]
      (is (= (->Sentence input-sentence [] #{(->Conjugation "tests") (->Conjugation "are") (->Conjugation "good")}) output-sentence)))))

(deftest parse-simple-paragraph
  (testing "Could not parse simple paragraph"
    (let [input-paragraph "This is. a paragraph."
          output-paragraph (parse-raw-paragraph input-paragraph)]
      (is (= output-paragraph (->Paragraph input-paragraph [(->Sentence "This is." [] #{(->Conjugation "this") (->Conjugation "is")})
                                                            (->Sentence "a paragraph." [] #{(->Conjugation "a") (->Conjugation"paragraph")})]))))))

(deftest parse-sentence-with-nested-sentence
  (testing "Incorrect-handeling-of-nested-sentence"
    (let [input-sentence   "tests \"cake tests\" are good."
          output-paragraph (parse-raw-paragraph input-sentence)]
        (is (= output-paragraph
            (->Paragraph "tests \"cake tests\" are good.",
                [(->Sentence "tests \"cake tests\" are good.",
                    [(->Paragraph "cake tests",
                    [(->Sentence "cake tests" [] #{(->Conjugation "cake") (->Conjugation "tests")})])
                    ]
                    #{(->Conjugation "cake") (->Conjugation "tests") (->Conjugation "are") (->Conjugation "good")}
            )]))))))


(deftest parse-single-line-text
  (testing "Incorrect parsing of a single-line text"
    (let [input-raw-text "This is a single line of text."
          output-text (parse-raw-text "test" input-raw-text)]
        (is (= (:title output-text)
               "test"))
        (is (= (count (:paragraphs output-text))
               1))
        (is (= (->> output-text :paragraphs first :raw)
               input-raw-text))
        )))

(deftest parse-multiple-line-text
  (testing "Incorrect parsing of multiple lines of text"
    (let [input-raw-text "This is\n multiple lines\nof text."
          output-text (parse-raw-text "test" input-raw-text)]
        (is (= (:title output-text)
            "test"))
        (is (= (count (:paragraphs output-text))
            3))
        (is (= (->> output-text :paragraphs (map :raw) set)
            #{"This is" "multiple lines" "of text."}))
        )))


(deftest parse-text-from-path
  (testing "Incorrect parsing of a text file"
    (let [text-path "test/lemmalearnerclj/test_files/single_line_text.txt"
          output-text (text-path->text text-path)]
        (is (= (:title output-text);(:title output-text)
               "single_line_text.txt"))
        (is (= (count (:paragraphs output-text))
               1))
        (is (= (->> output-text :paragraphs first :raw)
               "This is a single line.")))))
