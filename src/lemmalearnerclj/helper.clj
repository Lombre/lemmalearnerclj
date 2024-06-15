(ns lemmalearnerclj.helper
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]))

(defn record->map [record-collection]
  (walk/postwalk #(if (record? %) (into {} %) %) record-collection))

(defn preduce [f input-list]
  (let [partition-factor 16
        min-size 512
        count-dlist (count input-list)]
    (if (<= count-dlist min-size) (reducers/reduce f input-list)
        (let [partitions (partition-all (/ count-dlist partition-factor) input-list)
              divided-and-conquored (pmap #(preduce f %) partitions)]
          (reducers/reduce f divided-and-conquored)))))

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

(defn p-filter [f col]
  (let [partitions (partition-all (/ (count col) 32) col)]
    (flatten (pmap #(filter f %) partitions))))


(defn getx
  "Like two-argument get, but throws an exception if the key is
   not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info (str "Missing required key: " (with-out-str (println k)))
                      {:map m :key k})))))


(defn getx-in
  "Like two-argument get-in, but throws an exception if the key is
   not found."
  [m ks]
  (reduce getx m ks))
