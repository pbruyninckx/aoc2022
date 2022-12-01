(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map (fn [x] (Integer/parseInt x)) % ))))


(defn solve1 [v]
  (->> v
       (map #(reduce + %))
       (reduce max)))

(defn solve2 [v]
  (->> v
       (map #(reduce + %))
       (sort >)
       (take 3)
       (apply +)))

(defn -main []
  (let [input (parse-input "resources/input01.txt")]
    (println (solve1 input))
    (println (solve2 input))))
