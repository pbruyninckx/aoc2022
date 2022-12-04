(ns aoc.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [^String s]
  (->> (str/split s #"\D")
       (map #(Integer/parseInt %))
       (partition 2)
       (mapv vec)))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)))

(defn contains-range? [[a1 a2] [b1 b2]]
  (and (<= a1 b1) (<= b2 a2)))

(defn either-fully-contains? [a b]
  (or (contains-range? a b)
      (contains-range? b a)))

(defn solve [v]
  (->> v
       (filter #(apply either-fully-contains? %))
       count))

(defn -main []
  (let [input (parse-input "resources/input04.txt")]
    (println (solve input))))

