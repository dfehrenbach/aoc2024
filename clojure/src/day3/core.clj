(ns day3.core
  (:require [clojure.string :as string]))

(def mul-instruction "mul\\(\\d+,\\d+\\)")
(def mul-instruction-re (re-pattern mul-instruction))

(defn find-mul-instruction [line]
  (re-seq mul-instruction-re line))

(defn eval-mul-instruction [instruction]
  (let [numbers (re-seq #"\d+" instruction)]
    (apply * (map read-string numbers))))

(defn process-line [line]
  (let [instructions (find-mul-instruction line)]
    (reduce + (map eval-mul-instruction instructions))))

(def input
  (->> "src/day3/input.txt"
       slurp
       string/split-lines))

(defn part1 []
  (->> input
       (map process-line)
       (reduce +)))

(def do-instruction "do\\(\\)")
(def dont-instruction "don't\\(\\)")
(def all-instructions-re
  (->> [mul-instruction do-instruction dont-instruction]
       (map #(str "(" % ")"))
       (interpose "|")
       (apply str)
       re-pattern))

(defn find-all-instructions [line]
  (map first (re-seq all-instructions-re line)))

(defn eval-instructions [instruction]
  (if-let [numbers (re-seq #"\d+" instruction)]
    (apply * (map read-string numbers))
    (if (re-find (re-pattern do-instruction) instruction)
      true
      false)))

(defn collect-do-drop-dont
  ([vals] (collect-do-drop-dont [] vals))
  ([vals rest]
   (if (empty? rest) vals
       (let [[dovals dont-rest] (split-with (some-fn number? true?) rest)
             [_dont-vals dorest] (split-with (some-fn false? number?) dont-rest)]
         (recur (concat vals (filter number? dovals))
                (drop 1 dorest))))))

(defn collect-evaluated-instructions [line]
  (->> line
       (find-all-instructions)
       (map eval-instructions)))

(defn part2 []
  (->> input
       (mapcat collect-evaluated-instructions)
       collect-do-drop-dont
       (reduce +)))

(comment
  (part1)
  ;;=> 175700056

  (part2)
  ;;=> 71668682
  )
