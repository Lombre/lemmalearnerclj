(ns lemmalearnerclj.lemmatizer-test
  (:require
   [clojure.test :refer :all]
   [lemmalearnerclj.helper :as helper]
   [lemmalearnerclj.lemmatizer :refer :all]
   [lemmalearnerclj.textdatastructures])
  )


(deftest test-convert-json-to-lemma-map
  (testing "Cannot load a dictionary from a file"
    (let [json-lines (path->json-lines "dictionary-files/test/single-word.json")]
      (is (= ["kage" #{"kage"
                       "kagens"
                       "kagerne"
                       "kagers"
                       "kagernes"
                       "kagen"
                       "kager"
                       "kages"}]
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
      (is (= {"fiskene" "fisk", "fisks" "fisk", "kagens" "kage",
              "kage" "kage", "fisken" "fisk", "kagerne" "kage",
              "kagers" "kage", "kagernes" "kage", "kagen" "kage",
              "fiskenes" "fisk", "kager" "kage", "fisk" "fisk",
              "kages" "kage", "fiskens" "fisk"}
             (helper/record->map conjugation->lemma)))
      (is (= {"kage" #{"kagens" "kage" "kagerne" "kagers"
                       "kagernes" "kagen" "kager" "kages"},
              "fisk" #{"fiskene" "fisks" "fisken"
                       "fiskenes" "fisk" "fiskens"}}
             (helper/record->map lemma->conjugations)))
      (is (= {"kagens" #{"kage"}, "fiskene" #{"fisk"},
              "fisks" #{"fisk"}, "kage" #{"kage" "fisk"},
              "fisken" #{"fisk"}, "kagerne" #{"kage"},
              "kagers" #{"kage"}, "kagernes" #{"kage"},
              "kagen" #{"kage"}, "fisk" #{"fisk"},
              "kager" #{"kage"}, "fiskenes" #{"fisk"},
              "kages" #{"kage"}, "fiskens" #{"fisk"}}
             (helper/record->map conjugation->lemmas)))
      )))

(deftest test-is-same-saved-and-loaded-lemmafiles
  (testing ""
    (let [json-lines (path->json-lines "dictionary-files/test/two-words.json")
          {lemma->conjugations :lemma->conjugations} (json-lines->lemmatizer "test" json-lines)
          _ (save-lemma->conjugations "test/two-words" lemma->conjugations)
          loaded-lemma->words (load-saved-lemma-to-words-file "test/two-words")]
      (is (= lemma->conjugations loaded-lemma->words)))))


(deftest test-update-lemmatizer-with-personal-dictionary
  (testing ""
    (do
      ;; No additions should change nothing
      (is (= {:lemma->conjugations {"3" #{"1" "4"}}
              :conjugation->lemma {"1" "3" "4" "3"}}
             (update-lemmatizer-with-personal-dictionary {}
                                                         {:lemma->conjugations {"3" #{"1" "4"}}
                                                          :conjugation->lemma {"1" "3" "4" "3"}})))

      (is (= {:lemma->conjugations {"3" #{"4"} "2" #{"1" "2"}}
              :conjugation->lemma {"1" "2" "4" "3" "2" "2"}}
             (update-lemmatizer-with-personal-dictionary {"1" "2"}
                                                         {:lemma->conjugations {"3" #{"1" "4"}}
                                                          :conjugation->lemma {"1" "3" "4" "3"}})))

      (is (= {:lemma->conjugations {"3" #{"4"} "1" #{"1" "2"} "2" #{"5"}}
              :conjugation->lemma {"2" "1" "1" "1" "5" "2" "4" "3"}}
             (update-lemmatizer-with-personal-dictionary {"2" "1"}
                                                         {:lemma->conjugations {"3" #{"4"} "2" #{"1" "2" "5"}}
                                                          :conjugation->lemma {"1" "2" "2" "2" "5" "2" "4" "3"}})))
      )))

