(ns lemmalearnerclj.textdatastructures-test
  (:require [clojure.test :refer :all]
            [lemmalearnerclj.textdatastructures :refer :all])
  (:import [lemmalearnerclj.textdatastructures Text]))

(deftest test-text
  (testing "Cannot instantiate text"
    (is {:title "test" :paragraphs "text"} (Text. "test" "text"))))
