(ns aoc.day10
  (:require [clojure.string :as str]
            [clojure.pprint]))

(defn parse-line [^String line]
  (if (= \n (first line))
    :noop
    (Integer/parseInt (second (str/split line #" ")))))

(defn mod1 [num div]
  "Modulo for 1-based indexes, i.e. (mod (* N div) div) is div rather than 0"
  (inc (mod (dec num) div)))

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

(defn get-registers [commands]
  (->> commands
       (reduce add-output [1 1])
       (map-indexed (fn [i val] [(inc i) val]))))

(defn solve1 [registers]
  (->> registers
       (drop 19)
       (take-nth 40)
       (map #(apply * %))
       (take 6)
       (reduce +)))

(defn solve2 [registers]
  (->> registers
       (map #(update % 0 (fn [i] (mod1 i 40))))
       (map (fn [[i val]]
              (if (<= (- i 2) val i) "##" "  ")))
       (partition 40)
       (map str/join)))

(defn -main []
  (let [commands (parse-input "resources/input10.txt")
        registers (get-registers commands)]
    (println (solve1 registers))
    (clojure.pprint/pprint (solve2 registers))))
