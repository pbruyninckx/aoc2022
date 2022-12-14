(ns aoc.day09
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[direction steps] (str/split line #" ")]
    [(first direction) (Integer/parseInt steps)]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)))

(def moves
  {\U #(update % 1 inc)
   \D #(update % 1 dec)
   \L #(update % 0 dec)
   \R #(update % 0 inc)})

(defn follow [head tail]
  "Returns the new position of the tail, might be the same"
  (let [delta (map - head tail)]
    (if (> (reduce max (map abs delta)) 1)
      (->> (map - head tail)
           (map #(Integer/signum %))
           (map + tail))
     tail)))


(defn move-rope [[visited rope] direction]
  (let [rope
        (reduce (fn [moved-rope tail] (conj moved-rope (follow (peek moved-rope) tail)))
                [((moves direction) (first rope))]
                (rest rope))]
    [(conj visited (peek rope)) rope]))


(defn solve [rope-length instructions]
  (->> instructions
    (reduce (fn [[visited rope :as acc] [direction num-steps]]
              (nth (iterate #(move-rope % direction) acc) num-steps))
            [#{[0 0]} (into [] (repeat rope-length [0 0]))])
    first  ; visited
    count))


(defn -main []
  (let [input (parse-input "resources/input09.txt")]
    (doall (map #(println (solve % input)) [2 10]))))

