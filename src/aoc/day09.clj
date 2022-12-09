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


(defn move-step [[visited head tail] direction]
  (let [head ((moves direction) head)
        tail (follow head tail)]
    [(conj visited tail)
     head
     tail]))



(defn solve [instructions]
  (->> instructions
    (reduce (fn [[visited head tail :as acc] [direction num-steps]]
              (nth (iterate #(move-step % direction) acc) num-steps))
            [#{[0 0]} [0 0] [0 0]])
    first  ; visited
    count))



(defn -main []
  (let [input (parse-input "resources/input09.txt")]
    (println (solve input))))
