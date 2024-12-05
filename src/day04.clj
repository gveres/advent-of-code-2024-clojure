(ns day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def board
  (with-open [rdr (io/reader "inputs/day04-input.txt")]
    (doall (vec (line-seq rdr)))))

;; start from all x-es
;; extract each 4-long strings based on rules up/down/left/right/diagonals
;; count xmas-es

(defn char-at [[x y]]
  (get (get board y nil) x nil))

(defn v+ [[a b] [a' b']]
  [(+ a a') (+ b b')])

;; we have 8 options
(defn word-coords [v]
  (let [diffs [[[1 0] [2 0] [3 0]]
               [[-1 0] [-2 0] [-3 0]]
               [[0 1] [0 2] [0 3]]
               [[0 -1] [0 -2] [0 -3]]

               [[1 1] [2 2] [3 3]]
               [[-1 -1] [-2 -2] [-3 -3]]
               [[1 -1] [2 -2] [3 -3]]
               [[-1 1] [-2 2] [-3 3]]]]
    (for [dv diffs]
      (concat [v]
          (for [diff dv]
      (v+ v diff))))))

(defn extract-word [coords]
  (apply str (map char-at coords)))

(def all-words-ever
  (let [starting-coords (for [x (range (count (first board)))
                             y (range (count board)) ]
                         [x y])
        all-word-coords (mapcat word-coords starting-coords)]
    (map extract-word all-word-coords)
    ))

;;Q1
(count (filter #(= "XMAS" %) all-words-ever))
;; => 2496 -- OK


;;Q2
(defn x-mas-at [v]
  (let [diffs [[[1 1] [0 0] [-1 -1]]
               [[-1 -1] [0 0] [1 1]]
               [[1 -1] [0 0] [-1 1]]
               [[-1 1] [0 0] [1 -1]]]
        options (for [dv diffs] (map #(v+ v %) dv))]
    (= 2
       (->> (map extract-word options)
            (filter #(= % "MAS"))
            count))))

(def x-mas-es
  (filter true? (map x-mas-at (for [x (range (count (first board)))
                             y (range (count board)) ]
                         [x y]))))

(count x-mas-es)
;; => 1967 -- OK
