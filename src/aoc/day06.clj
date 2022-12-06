(ns aoc.day06
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/trim))

(defn solve [N s]
  (->> s
       (partition N 1)
       (map (comp count set))
       (map-indexed vector)
       (filter (fn [[_ num-el]] (= num-el N)))
       first   ; first [index num-el] with num-el = N
       first   ; we only want the index
       (+ N))) ; we want the index after the message

(defn -main []
  (let [input (parse-input "resources/input06.txt")]
    (dorun (map #(println (solve % input))
                [4 14]))))

