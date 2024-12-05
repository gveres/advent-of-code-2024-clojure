(ns day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "inputs/day01-input.txt")]
             (doall (line-seq rdr))))

(def line (first lines))

(defn line->vec [line]
  (-> line
   (clojure.string/split #"\s+")
   (->> (mapv #(Long/parseLong %)))))

(defn rows->cols [data]
  (reduce
   (fn [[f s] [f' s']] [(conj f f') (conj s s')])
   [[][]]
   data))

(def cols (rows->cols (map line->vec lines)))
(def sorted-cols (map sort cols))
(def diffs (map (fn [a b] (abs (- a b))) (first sorted-cols) (second sorted-cols)))

(reduce + diffs)
;; => 1941353 - correct

(def right-freqs (frequencies (second cols)))

(defn right-freq-of [x] (get right-freqs x 0))

(def res
  (reduce + (map (fn [x] (* x (right-freq-of x))) (first cols))))

res
;; => 22539317 - correct
