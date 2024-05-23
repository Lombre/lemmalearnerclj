(ns lemmalearnerclj.helper
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.walk :as walk]))

(defn record->map [record-collection]
  (walk/postwalk #(if (record? %) (into {} %) %) record-collection))

(defn p-divide-and-conquor [f partition-factor input-list & {:keys [min-size] :or {min-size 1000}}]
  (let [count-dlist (count input-list)]
    (if (< count-dlist min-size) (reducers/fold f input-list)
        (let [divided-and-conquored (pmap #(p-divide-and-conquor f partition-factor %)
                                          (partition (/ count-dlist partition-factor) (/ count-dlist partition-factor) nil input-list))]
          (reducers/fold f divided-and-conquored)))))

(defmacro wrap-with-print [before expression after]
  `(do (println ~before)
       (let [expression# ~expression]
         (println ~after)
         expression#)))

(defmacro print-after [expression after]
  `(let [expression# ~expression]
     (println ~after)
     expression#)
  )

(defmacro print-threading [message expression]
  `(let [expression# ~expression]
     (println ~message)
     expression#)
  )

(defn print-if [condition message]
  (if condition
    (println message)
    nil))
