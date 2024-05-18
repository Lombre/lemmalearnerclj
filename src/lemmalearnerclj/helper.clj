(ns lemmalearnerclj.helper
  (:require
   [clojure.walk :as walk]))

(defn record->map [record-collection]
  (walk/postwalk #(if (record? %) (into {} %) %) record-collection))

(defmacro wrap-with-print [before expression after]
  `(do (println ~before)
       (let [expression# ~expression]
         (println ~after)
         expression#)))
