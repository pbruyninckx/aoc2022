(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map (fn [x] (Integer/parseInt x)) % ))))


(defn solve [v]
  (->> v
       (map #(reduce + %))
       (reduce max)))

(defn -main []
  (let [input (parse-input "resources/input01.txt")]
    (println (solve input))))
