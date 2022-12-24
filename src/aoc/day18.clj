(ns aoc.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

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

(defn flatten-1 [xs]
  (apply concat xs))

(defn extend [locations]
  (into locations
        (flatten-1 (map neighbours locations))))

(defn shrink [keep-locations extended-locations]
  (->> extended-locations
       (filter (fn [l] (= (count (set/intersection (set (neighbours l))
                                                   extended-locations))
                          6)))
       (into keep-locations)))

(defn fill [locations]
  (->> locations
       (iterate extend)
       (drop 12)
       first
       (iterate (partial shrink locations))
       (drop 20)
       first))

(defn count-surface [locations]
  (->> locations
       (reduce (fn [[seen num-sides] loc]
                 [(conj seen loc)
                  (+ num-sides
                     (- (* 2 (count (set/difference (neighbours loc) seen))) 6))])
               [#{} 0])
       second))

(defn count-external-surface [locations]
  (count-surface (fill (set locations))))

(defn -main []
  (let [locations (parse-input "resources/input18.txt")]
    (doseq [solve [count-surface count-external-surface]]
      (println (solve locations)))))

