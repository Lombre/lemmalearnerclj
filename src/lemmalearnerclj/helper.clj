(ns lemmalearnerclj.helper
  (:require
   [clojure.walk :as walk]))

(defn record->map [record-collection]
  (walk/postwalk #(if (record? %) (into {} %) %) record-collection))
