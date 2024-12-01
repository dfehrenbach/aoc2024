(ns day12.core
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (let [[springs groups] (string/split line #" ")
        groups (map read-string (re-seq #"[0-9]+" groups))]
    {:springs springs
     :groups groups}))

(defn get-input [path]
  (->> path
       slurp
       string/split-lines
       (map parse-line)))

(def test-input
  (get-input "src/day12/test.txt"))

(def input
  (get-input "src/day12/input.txt"))

(def find-arrangements
  (memoize (fn [springs groups groupcnt]
             (if (empty? springs)
               (if (and (empty? groups) (zero? groupcnt))
                 1 0)
               (let [character (first springs)
                     group (or (first groups) 0)]
                 (+
                  (if (some #{"#" "?"} [character])
                    (find-arrangements (rest springs) groups (inc groupcnt))
                    0)
                  (if (some #{"." "?"} [character])
                    (if (pos? groupcnt)
                      (if (not= groupcnt group) 0
                          (find-arrangements (rest springs) (rest groups) 0))
                      (find-arrangements (rest springs) groups 0))
                    0)))))))

(defn part1 [input]
  (->> input
       (pmap (fn [{:keys [springs groups]}]
               (find-arrangements (string/split (str springs ".") #"") groups 0)))
       (reduce +)))

(defn expand-input [input]
  (->> input
       (mapv (fn [{:keys [springs groups]}]
               {:springs (string/join "?" (repeat 5 springs))
                :groups (vec (flatten (repeat 5 groups)))}))))

(defn part2 [input]
  (->> input
       expand-input
       (pmap (fn [{:keys [springs groups]}]
               (find-arrangements (string/split (str springs ".") #"") groups 0)))
       (reduce +)))

(comment
  (first test-input)
  ;; => {:springs "???.###", :groups [1 1 3]}

  (find-arrangements (string/split (str (:springs (first test-input)) ".") #"") (:groups (first test-input)) 0)
  ;; => 1

  (find-arrangements (string/split (str (:springs (second test-input)) ".") #"") (:groups (second test-input)) 0)
  ;; => 4

  (part1 test-input)
  ;; => 21

  (part1 input)
  ;; => 7204

  (part2 test-input)
  ;; => 525152

  (part2 input)
  ;; => 1672318386674

  0)

(defn calc-diff-change-at-100 [currentvotes, currentvotes2, percentage-reporting]
  (let [votes-at-100 (* currentvotes (/ 100 percentage-reporting))
        votes-at-1002 (* currentvotes2 (/ 100 percentage-reporting))]
    (- (- votes-at-100 votes-at-1002) (- currentvotes currentvotes2))))

(comment
  (+ (calc-diff-change-at-100 419051 279292 94.4)
     (calc-diff-change-at-100 64268 52353 78.7)
     (calc-diff-change-at-100 279370 177632 87.3)
     (calc-diff-change-at-100 147903 120020 85.3)
     (calc-diff-change-at-100 194932 121148 92.6)
     (calc-diff-change-at-100 484929 128645 78.1))
  ;;=> 136922.9384937583
  )
