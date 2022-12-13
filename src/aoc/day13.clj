(ns aoc.day13
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (filter (complement empty?))
       (map read-string)
       (partition 2)))

(defn recursive-compare [left right]
  "Returns negative, zero, positive if left is less/equal/great than right"
  (cond
    (and (int? left) (int? right))
    (compare left right)
    (int? left)
    (recur [left] right)
    (int? right)
    (recur left [right])
    (and (empty? left) (empty? right))
    0
    (empty? left)
    -1
    (empty? right)
    1
    :else
    (loop [[l & lrest] left
           [r & rrest] right]
      (let [result (recursive-compare l r)]
        (if (and (= result 0) (or lrest rrest))
          (recur lrest rrest)
          result)))))


(defn solve [input]
  (->> input
       (map (partial apply recursive-compare))
       (map-indexed vector)
       (filter (fn [[_ ret]] (<= ret 0)))
       (map (comp inc first))  ; keep index - start at 1
       (reduce +)))

(defn -main []
  (let [input (parse-input "resources/input13.txt")]
    (println (solve input))))
