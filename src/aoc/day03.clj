(ns aoc.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines))

(defn priority [^Character c]
  (let [i (int c)]
    (if (<= i 90)
      (- i 38)
      (- i 96))))

(defn split-in-halves [s]
  (split-at (/ (count s) 2) s))

(defn get-common [backpacks]
  (->> backpacks
       (map set)
       (apply set/intersection)
       first))

(defn solve [part v]
  (->> v
       ((if (= part 1)
          #(map split-in-halves %)
          #(partition 3 %)))
       (map get-common)
       (map priority)
       (reduce +)))

(defn -main []
  (let [input (parse-input "resources/input03.txt")]
    (println (solve 1 input))
    (println (solve 2 input))))

