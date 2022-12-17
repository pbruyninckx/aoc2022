(ns aoc.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn flatten-1 [xs]
  (apply concat xs))

(defn parse-line [line]
  (->> (str/split line #"\D+")
       (map parse-long)
       (partition 2)
       (partition 2 1)))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)
       flatten-1))

(defn inclusive-range [a b]
  (range (min a b) (inc (max a b))))

(defn draw-rock-path [[[c1 r1] [c2 r2]]]
  "Creates the paths as {column #{row}}"
  (if (= c1 c2)
      {c1 (into #{} (inclusive-range r1 r2))}
      (into {} (map (fn [c] [c #{r1}]) (inclusive-range c1 c2)))))

(defn compact-representation [column]
  "Convert a sequence of numbers into a vector of tuples.
   E.g. [1 2 3 5 6 8] becomes [[1 3] [5 6] [8 8]]"
  (reduce (fn [ret value]
            (if (= (dec value) (peek (peek ret)))
              (update-in ret [(dec (count ret)) 1] inc)
              (conj ret [value value])))
            []
            (sort column)))

(defn draw-paths [paths]
  (->> paths
       (map draw-rock-path)
       (reduce (partial merge-with set/union))))

(defn get-compact-input [input]
  "Represents the input as {row (compact-representation column)}"
  (update-vals (draw-paths input) compact-representation))

(defn get-stop-pos [cave-col row]
  (if-let [[top _bottom] (->> cave-col
                             (drop-while (fn [[_top bottom]] (< bottom row)))
                             first)]
    (if (< row top)
      (dec top)
      :blocked)
    :abyss))

(defn add-sand-to-col [cave-col row]
  (let [index
        (->> cave-col
             (map-indexed vector)
             (drop-while (fn [[_ [_top bottom]]] (< bottom row)))
             first
             first)]
    (update-in cave-col [index 0] dec)))

(defn slide-pos [[col row]]
  [[(dec col) (inc row)]
   [(inc col) (inc row)]])

(defn add-sand [cave [col row :as pos]]
  (if-let [cave-col (cave col)]
    (let [stop-pos (get-stop-pos cave-col row)]
      (case stop-pos
        :abyss :abyss
        :blocked :blocked
        (if-let [continued (->> (slide-pos [col stop-pos])
                                (map (partial add-sand cave))
                                (filter (partial not= :blocked))
                                first)]
          continued
          (update cave col #(add-sand-to-col % row)))))
    :abyss
    ))

(def start-point [500 0])

(defn solve1 [input]
  (->> (get-compact-input input)
       (iterate #(add-sand % start-point))
       (map-indexed vector)
       (filter (fn [[_i cave]] (keyword? cave)))
       ((comp dec first first))))

(defn add-floor [input]
  (let [floor-y (->> input flatten-1 (map second) (reduce max) (+ 2))]
    (cons [[(- (start-point 0) floor-y 10) floor-y]
           [(+ (start-point 0) floor-y 10) floor-y]] input)))

(defn solve2 [input]
  (->> (add-floor input)
       get-compact-input
       (iterate #(add-sand % start-point))
       (map-indexed vector)
       (filter (fn [[_i cave]] (= 0 (get-in cave [(start-point 0) 0 0]))))
       ((comp first first))))


(defn -main []
  (let [input (parse-input "resources/input14.txt")]
    (doseq [solve [solve1 solve2]]
      (println (solve input)))))
