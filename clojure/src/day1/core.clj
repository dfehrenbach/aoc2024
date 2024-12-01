(ns day1.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day1/input.txt"
       slurp
       string/split-lines))

(defn read-line [line]
  (let [[first second] (re-seq  #"\d+" line)]
    [(read-string first) (read-string second)]))

(defn find-diffs [input]
  (let [first-col (sort (map first input))
        second-col (sort (map second input))]
    (map (comp Math/abs -) second-col first-col)))


(defn part1 []
;; take each line, find the first number and last number, concatenate them, interpret as a number, then sum all
  (->> input
       (map read-line)
       find-diffs
       (reduce +)))

(defn find-similarity-scores [input]
  (let [first-col (map first input)
        second-col (map second input)
        number->count (frequencies second-col)]
    (map (fn [n] (* n (get number->count n 0)))
         first-col)))

(defn part2 []
;; take each line, find the first number and last number, concatenate them, interpret as a number, then sum all
  (->> input
       (map read-line)
       find-similarity-scores
       (reduce +)))

(comment
  input

  (part1)
  ;;=> 2113135

  (part2)
  ;;=> 19097157
  )
