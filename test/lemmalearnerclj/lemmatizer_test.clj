(ns lemmalearnerclj.lemmatizer-test
  (:require
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.lemmatizer :refer :all]
   [lemmalearnerclj.textdatastructures])
  (:import
   [lemmalearnerclj.textdatastructures Conjugation Lemma]))


(deftest test-convert-json-to-lemma-map
  (testing "Cannot load a dictionary from a file"
    (let [json-lines (path->json-lines "dictionary-files/test/single-word.json")]
      (is (= [(Lemma. "kage") #{(Conjugation. "kage")
                                (Conjugation. "kagens")
                                (Conjugation. "kagerne")
                                (Conjugation. "kagers")
                                (Conjugation. "kagernes")
                                (Conjugation. "kagen")
                                (Conjugation. "kager")
                                (Conjugation. "kages")}]
             (->> json-lines first jsonobj->lemmamap))))))

(deftest test-json-lines->lemmatizer
  (testing ""
    (let [json-lines (path->json-lines "dictionary-files/test/two-words.json")
          {language :language
           conjugation->lemma :conjugation->lemma
           conjugation->lemmas :conjugation->lemmas
           lemma->conjugations :lemma->conjugations} (json-lines->lemmatizer "test" json-lines)]
      (is (= "test"
             language))
      (is (= {{:raw "fiskene"} {:raw "fisk"}, {:raw "fisks"} {:raw "fisk"}, {:raw "kagens"} {:raw "kage"},
              {:raw "kage"} {:raw "kage"}, {:raw "fisken"} {:raw "fisk"}, {:raw "kagerne"} {:raw "kage"},
              {:raw "kagers"} {:raw "kage"}, {:raw "kagernes"} {:raw "kage"}, {:raw "kagen"} {:raw "kage"},
              {:raw "fiskenes"} {:raw "fisk"}, {:raw "kager"} {:raw "kage"}, {:raw "fisk"} {:raw "fisk"},
              {:raw "kages"} {:raw "kage"}, {:raw "fiskens"} {:raw "fisk"}}
             (helper/record->map conjugation->lemma)))
      (is (= {{:raw "kage"} #{{:raw "kagens"} {:raw "kage"} {:raw "kagerne"} {:raw "kagers"}
                              {:raw "kagernes"} {:raw "kagen"} {:raw "kager"} {:raw "kages"}},
              {:raw "fisk"} #{{:raw "fiskene"} {:raw "fisks"} {:raw "fisken"} {:raw "kagen"}
                              {:raw "fiskenes"} {:raw "fisk"} {:raw "fiskens"}}}
             (helper/record->map lemma->conjugations)))
      (is (= {{:raw "kagens"} #{{:raw "kage"}}, {:raw "fiskene"} #{{:raw "fisk"}},
              {:raw "fisks"} #{{:raw "fisk"}}, {:raw "kage"} #{{:raw "kage"}},
              {:raw "fisken"} #{{:raw "fisk"}}, {:raw "kagerne"} #{{:raw "kage"}},
              {:raw "kagers"} #{{:raw "kage"}}, {:raw "kagernes"} #{{:raw "kage"}},
              {:raw "kagen"} #{{:raw "kage"} {:raw "fisk"}}, {:raw "fisk"} #{{:raw "fisk"}},
              {:raw "kager"} #{{:raw "kage"}}, {:raw "fiskenes"} #{{:raw "fisk"}},
              {:raw "kages"} #{{:raw "kage"}}, {:raw "fiskens"} #{{:raw "fisk"}}}
             (helper/record->map conjugation->lemmas)))
      )))

(deftest test-is-same-saved-and-loaded-lemmafiles
  (testing ""
    (let [json-lines (path->json-lines "dictionary-files/test/two-words.json")
          {lemma->conjugations :lemma->conjugations} (json-lines->lemmatizer "test" json-lines)
          _ (save-lemma->words "test/two-words" lemma->conjugations)
          loaded-lemma->words (load-saved-lemma-to-words-file "test/two-words")]
      (is (= lemma->conjugations loaded-lemma->words)))))
