(ns day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def instructions
  (slurp "inputs/day03-input.txt"))

(def uncorrupted-muls (re-seq #"mul\((\d+),(\d+)\)" instructions))

(def mul-results
  (map
   (fn [[_ x y]]
     (* (read-string x) (read-string y)))
   uncorrupted-muls))


;;Q1
(apply + mul-results)
;; => 178538786 -- OK

(def uncorrupted-instructions
  (re-seq #"mul\(\d+,\d+\)|do(?:n't)?\(\)" instructions))

(def do-dont-segments (partition-by #(.startsWith % "d")  uncorrupted-instructions))

;; ugly-ass imperative solution

(defn calc-mul-results [instructions]
  (println "will calculate " instructions)
  (let [processed-instructions (re-seq #"mul\((\d+),(\d+)\)" instructions)]
  (map
   (fn [[_ x y]]
     (* (read-string x) (read-string y)))
   processed-instructions)))

(def muls-to-do
  (let [do-flag (atom true)]
    (for [instructions do-dont-segments]
      (cond
        (= (first instructions) "do()") (do (reset! do-flag true) 0)
        (= (first instructions) "don't()") (do (reset! do-flag false) 0)
        @do-flag (calc-mul-results (apply str instructions))
        :else 0))))

;;Q2
(apply + (flatten muls-to-do))
;; => 102467299 -- OK
