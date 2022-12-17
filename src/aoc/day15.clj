(ns aoc.day15
  (:require [clojure.string :as str]))

(defn parse-line [line]
  "Returns [sx sy bx by]"
  (->> line
       (re-seq #"[0-9-]+")
       (mapv parse-long)))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)))

(defn beacon-free [[sx sy bx by] target-row]
  "Returns the beacon free range of the target row,
   or nil if there's no overlap.
   Ignores known beacons."
  (let [dist (apply + (map abs [(- sx bx) (- sy by)]))
        dx (- dist (abs (- target-row sy)))]
    (if (>= dx 0)
      [(- sx dx) (+ sx dx)])))

(defn positions-covered [ranges-covered]
  (let [starts (map (fn [r] [(first r) :start]) ranges-covered)
        ends (map (fn [r] [(second r) :end]) ranges-covered)
        start-and-ends (sort-by first (concat starts ends))]
    (->> start-and-ends
         (reduce (fn [[result current-start active-ranges] [val mark]]
                   (cond
                     (= active-ranges 0)
                     [result val 1]
                     (and (= mark :end) (= active-ranges 1))
                     [(+ result (- val current-start) 1) nil 0]
                     :else
                     [result current-start ((if (= mark :start) inc dec) active-ranges)]))
                 [0 nil 0])
         first)))

(defn solve [sensor-info target-row]
  (let [beacon-free-positions (->> sensor-info
                                   (map #(beacon-free % target-row))
                                   (filter identity)
                                   positions-covered)
        actual-beacons (->> sensor-info
                            (filter #(= (peek %) target-row))
                            (map #(get % 2))
                            distinct
                            count)]
    (- beacon-free-positions actual-beacons)))

(defn -main []
  (let [sensor-info (parse-input "resources/input15.txt")
        target-row 2000000]
    (println (solve sensor-info target-row))))
