(ns day5.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day5/input.txt"
       slurp
       string/split-lines
       (split-with #(not (string/blank? %)))))

(defn convert-rules-to-map [rules]
  (reduce (fn [acc raw-rule]
            (let [[first second] (re-seq #"\d+" raw-rule)]
              (update acc first #(conj % second))))
          {}
          rules))

(defn convert-update-order-to-vector [update-order]
  (vec (mapv #(vec (string/split % #",")) update-order)))

(defn my-rules-comparator [rules]
  ;; if a is in after-bs, return false
  ;; if b is in after-as, return true
  (fn [a b]
    (let [after-as (get rules a)
          after-bs (get rules b)]
      (if (contains? (set after-as) b) true
          (not (contains? (set after-bs) a))))))

(defn by-rules-sort [rules]
  (fn [a b]
    (. (my-rules-comparator rules) compare a b)))

(defn is-in-order? [rules update-order]
  (let [sorted (sort (by-rules-sort rules) update-order)]
    (= sorted update-order)))

(defn collect-in-order-updates [rules update-order]
  (when (is-in-order? rules update-order)
    update-order))

(defn get-middle-value [update-order]
  (let [middle-index (int (/ (count update-order) 2))]
    (get update-order middle-index)))

(defn part1 []
  (let [ordering-rules (convert-rules-to-map (first input))
        update-order (convert-update-order-to-vector (drop 1 (second input)))]
    (->> update-order
         (filter #(collect-in-order-updates ordering-rules %))
         (map get-middle-value)
         (map read-string)
         (reduce +))))

(defn collect-out-of-order-updates [rules update-order]
  (when-not (is-in-order? rules update-order)
    update-order))

(defn part2 []
  (let [ordering-rules (convert-rules-to-map (first input))
        update-order (convert-update-order-to-vector (drop 1 (second input)))]
    (->> update-order
         (filter #(collect-out-of-order-updates ordering-rules %))
         (mapv #(vec (sort (by-rules-sort ordering-rules) %)))
         (mapv get-middle-value)
         (map read-string)
         (reduce +))))

(comment
  (part1)
  ;;=> 5713

  (part2)
  ;;=> 5180
  0)
