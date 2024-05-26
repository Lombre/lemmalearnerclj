(ns lemmalearnerclj.helper
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.walk :as walk]
   [parallel.core :as p]))

(defn record->map [record-collection]
  (walk/postwalk #(if (record? %) (into {} %) %) record-collection))

(defn p-dac-reduce [f input-list & {:keys [partition-factor min-size] :or {partition-factor 4 min-size 1024}}]
  (let [count-dlist (count input-list)]
    (if (< count-dlist min-size) (reducers/fold f input-list)
        (let [divided-and-conquored (pmap #(p-dac-reduce f % :partition-factor partition-factor :min-size min-size)
                                          (partition (/ count-dlist partition-factor) (/ count-dlist partition-factor) nil input-list))]
          (reducers/fold f divided-and-conquored)))))

(defn p-dac-map [f input-list & {:keys [partition-factor min-size] :or {partition-factor 4 min-size 1024}}]
  (let [count-dlist (count input-list)]
    (if (< count-dlist min-size) (pmap f input-list)
        (let [divided (partition (/ count-dlist partition-factor) (/ count-dlist partition-factor) nil input-list)
              divided-and-conquored (pmap #(p-dac-map f % :partition-factor partition-factor :min-size min-size) divided)]
          (doall (flatten divided-and-conquored))))))

(defn p-filter [f list]
  (->> list
       (pmap #(identity [% (f %)]))
       (filter #(nth % 1))
       (pmap #(nth % 0))
       (doall)))

(defmacro wrap-with-print [before expression after]
  `(do (println ~before)
       (let [expression# ~expression]
         (println ~after)
         expression#)))

(defmacro print-after [expression after]
  `(let [expression# ~expression]
     (println ~after)
     expression#))

(defn map-map [input-map col]
  (map #(get input-map %) col))

(defmacro sectime
  [expr]
  `(let [start# (. System (currentTimeMillis))
         ret# ~expr]
     (prn (str "Elapsed time: " (/ (double (- (. System (currentTimeMillis)) start#)) 1000.0) " secs"))
     ret#))

(defmacro print-threading [message expression]
  `(let [expression# ~expression]
     (println ~message)
     expression#)
  )

(defn print-if [condition & messages]
  (if condition
    (apply println messages)
    nil))

