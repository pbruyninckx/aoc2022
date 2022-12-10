(ns aoc.day10
  (:require [clojure.string :as str]))

(defn parse-line [^String line]
  (if (= \n (first line))
    :noop
    (Integer/parseInt (second (str/split line #" ")))))

(defn parse-input [^String f]
  (->> f
       slurp
       (str/split-lines)
       (map parse-line)))

(defn add-output [register command]
  (let [X (peek register)]
    (if (= command :noop)
      (conj register X)
      (into register (repeat 2 (+ X command))))))

(defn solve [commands]
  (->> commands
       (reduce add-output [1])
       (map-indexed (fn [i val] [(+ i 2) val]))
       (drop 18)
       (take-nth 40)
       (map #(apply * %))
       (take 6)
       (reduce +)))

(defn -main []
  (let [input (parse-input "resources/input10.txt")]
    (println (solve input))))
