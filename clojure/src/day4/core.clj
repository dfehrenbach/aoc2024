(ns day4.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day4/input.txt"
       slurp
       string/split-lines
       (mapv #(string/split % #""))
       vec))

(defn xmas-right? [arr y x]
  (let [xs (range x (+ x 4))]
    (= (apply str
              (for [x xs]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-left? [arr y x]
  (let [xs (reverse (range (- x 3) (inc x)))]
    (= (apply str
              (for [x xs]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-up? [arr y x]
  (let [ys (reverse (range (- y 3) (inc y)))]
    (= (apply str
              (for [y ys]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-down? [arr y x]
  (let [ys (range y (+ y 4))]
    (= (apply str
              (for [y ys]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-diagonal-down-right? [arr y x]
  (let [ys (range y (+ y 4))
        xs (range x (+ x 4))]
    (= (apply str
              (for [[y x] (map vector ys xs)]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-diagonal-up-right? [arr y x]
  (let [ys (reverse (range (- y 3) (inc y)))
        xs (range x (+ x 4))]
    (= (apply str
              (for [[y x] (map vector ys xs)]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-diagonal-down-left? [arr y x]
  (let [ys (range y (+ y 4))
        xs (reverse (range (- x 3) (inc x)))]
    (= (apply str
              (for [[y x] (map vector ys xs)]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-diagonal-up-left? [arr y x]
  (let [ys (reverse (range (- y 3) (inc y)))
        xs (reverse (range (- x 3) (inc x)))]
    (= (apply str
              (for [[y x] (map vector ys xs)]
                (get-in arr [y x])))
       "XMAS")))

(defn xmas-count [arr y x]
  (count (filter true?
                 [(xmas-right? arr y x)
                  (xmas-left? arr y x)
                  (xmas-up? arr y x)
                  (xmas-down? arr y x)
                  (xmas-diagonal-down-right? arr y x)
                  (xmas-diagonal-up-right? arr y x)
                  (xmas-diagonal-down-left? arr y x)
                  (xmas-diagonal-up-left? arr y x)])))

(defn scan-array [arr]
  (for [y (range (count arr))
        x (range (count (first arr)))]
    (xmas-count arr y x)))

(defn part1 []
  (->> input
       scan-array
       (reduce +)))

(defn x-mas? [arr y x]
  (when (= "A" (get-in arr [y x]))
    (let [[backward1 backward2] [[(dec y) (dec x)] [(inc y) (inc x)]]
          [forward1 forward2] [[(dec y) (inc x)] [(inc y) (dec x)]]]
      (and
       (or
        (and (= "S" (get-in arr forward1)) (= "M" (get-in arr forward2)))
        (and (= "M" (get-in arr forward1)) (= "S" (get-in arr forward2))))
       (or
        (and (= "S" (get-in arr backward1)) (= "M" (get-in arr backward2)))
        (and (= "M" (get-in arr backward1)) (= "S" (get-in arr backward2))))))))

(defn scan-array2 [arr]
  (for [y (range (count arr))
        x (range (count (first arr)))
        :when (= "A" (get-in arr [y x]))]
    (x-mas? arr y x)))

(defn part2 []
  (->> input
       scan-array2
       (filter true?)
       count))

(comment
  (part1)
  ;;=> 2618

  (part2)
  ;;=> 2011
  )
