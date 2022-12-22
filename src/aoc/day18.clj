(ns aoc.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map (fn [line] (mapv parse-long (str/split line #","))))))

(defn neighbours [location]
  (into #{}
      (for [fun [inc dec]
            pos (range 3)]
        (update location pos fun))))

(defn solve [locations]
  (->> locations
       (reduce (fn [[seen num-sides] loc]
                 [(conj seen loc)
                  (+ num-sides (- (* 2 (count (set/difference (neighbours loc) seen))) 6))])
               [#{} 0])
       second))

(defn -main []
  (let [locations (parse-input "resources/input18.txt")]
    (println (solve locations))))

