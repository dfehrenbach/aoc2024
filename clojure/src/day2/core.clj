(ns day2.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day2/input.txt"
       slurp
       string/split-lines))

(defn read-report [line]
  (let [levels (re-seq  #"\d+" line)]
    (mapv read-string levels)))

(defn check-increasing [levels]
  (apply <= levels))

(defn check-decreasing [levels]
  (apply >= levels))

(defn adjacent-diff-between-one-and-three [levels]
  (every? (fn [[n1 n2]] (<= 1 (Math/abs (- n2 n1)) 3))
          (partition 2 1 levels)))

(def is-safe
  (every-pred
   (some-fn check-increasing
            check-decreasing)
   adjacent-diff-between-one-and-three))

(defn part1 []
  (->> input
       (map read-report)
       (map is-safe)
       (filter true?)
       count))

(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn check-safety-for-all-levels [levels]
  ;; remove one level, check if the rest is safe, repeat for the next level and check again
  (some true?
        (->> (range (count levels))
             (map (fn [i]
                    (let [trimmed-levels (vec-remove i levels)]
                      (is-safe trimmed-levels)))))))

(defn part2 []
  (->> input
       (mapv read-report)
       (mapv check-safety-for-all-levels)
       (filter true?)
       count))

(comment
  (part1)
  ;;=> 402
  (part2)
  ;;=> 455
  )
