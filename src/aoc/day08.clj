(ns aoc.day08
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (mapv #(mapv (fn [x] (Character/digit x 10)) % ))))

(defn is-visible [row]
  (->> row
    (reduce (fn [{:keys [result highest]} value]
              {:result (conj result (> value highest))
               :highest (max highest value)})
            {:result []
             :highest -1})
    :result))

(defn transpose [m]
  (apply mapv vector m))

(defn mat-op [op m1 m2]
  (mapv #(mapv op %1 %2) m1 m2))

(defn solve [input]
  (let [v1 (mapv is-visible input)
        v2 (mapv (comp vec reverse is-visible reverse) input)
        v3 (transpose (mapv is-visible (transpose input)))
        v4 (transpose (mapv (comp vec reverse is-visible reverse) (transpose input)))
        v  (reduce (partial mat-op #(or %1 %2)) [v1 v2 v3 v4])]
    (->> v
         (map #(count (filter true? %)))
         (reduce +))))
        

(defn -main []
  (let [input (parse-input "resources/input08.txt")]
    (println (solve input))))
