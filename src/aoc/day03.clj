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

(defn get-common [s]
  (let [halves (split-at (/ (count s) 2) s)]
    (->> halves
         (map set)
         (apply set/intersection)
         first)))

(defn solve [v]
  (->> v
       (map get-common)
       (map priority)
       (reduce +)))


(defn -main []
  (let [input (parse-input "resources/input03.txt")]
    (println (solve input))))

