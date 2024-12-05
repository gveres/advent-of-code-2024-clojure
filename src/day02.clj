(ns day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def reports (with-open [rdr (io/reader "inputs/day02-input.txt")]
             (doall (line-seq rdr))))

(defn report->vec [line]
  (-> line
   (clojure.string/split #"\s+")
   (->> (mapv #(Long/parseLong %)))))

(def report-seqs (map report->vec reports))

(defn safe? [report]
  (let [neighbors (partition 2 1 report)
        diffs (map (partial apply -) neighbors)
        monotone-changing? (or (every? pos? diffs) (every? neg? diffs))
        largest-gap-okay? (every? true? (map #(>= 3 (abs %) 1) diffs))]
    (and monotone-changing? largest-gap-okay?)))

;; Q1
(count (filter safe? report-seqs))
;; => 402 - OK

(defn drop-nth [s n]
  (keep-indexed #(when (not= %1 n) %2) s))


;; let's brute force
(defn can-be-made-safe? [report]
  (let [variants (for [i (range 0 (count report))]
                   (drop-nth report i))]
    (some safe? variants)))

;;Q2
(count (filter #(or (safe? %) (can-be-made-safe? %)) report-seqs))
;; => 455 - OK
