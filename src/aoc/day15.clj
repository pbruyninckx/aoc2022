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

(defn clip-range [[a b] [low high]]
  "Maps a range so it fits within [low high], or returns nil if it falls outside"
  (if (or (< b low) (> a high))
    nil
    [(max a low) (min b high)]))

(defn solve2 [sensor-info max-coord]
  (let [rows-ranges (map (fn [r] (->> sensor-info
                                      (map #(beacon-free % r))
                                      (filter identity)
                                      (map #(clip-range % [0 max-coord]))
                                      (filter identity)))
                         (range (inc max-coord)))
        [row-index row-ranges] (->> rows-ranges
                                    (map-indexed vector)
                                    (filter #(= (positions-covered (second %)) max-coord))
                                    first)
        starts (into #{} (sort (map first row-ranges)))
        ends (into #{} (sort (map second row-ranges)))
        col-index (->> ends
                       (map inc)
                       (filter (fn [e] (starts (inc e))))
                       first)]
    (+ (* 4000000 col-index) row-index)))


(defn -main []
  (let [sensor-info (parse-input "resources/input15.txt")
        target-row 2000000
        max-coord 4000000]
    (println (solve sensor-info target-row))
    (println (solve2 sensor-info max-coord))))
