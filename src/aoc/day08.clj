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

(defn visible-from [seen ind height]
  (let [blockers (subvec seen height)
        block-ind (apply max blockers)]
    (- ind block-ind)))

(defn num-visible [row]
  (->> row
       (map-indexed vector)
    (reduce 
      (fn [{:keys [seen result]} [ind value]] 
        {:seen (assoc seen value ind) 
         :result (conj result (visible-from seen ind value))}) 
      {:seen (vec (repeat 10 0))
       :result []})
    :result))

(defn transpose [m]
  (apply mapv vector m))

(defn mat-op [op m1 m2]
  (mapv #(mapv op %1 %2) m1 m2))

(defn compute-visibility [mat row-op acc-op]
  (let [v1 (mapv row-op mat)
        v2 (mapv (comp vec reverse row-op reverse) mat)
        v3 (transpose (mapv row-op (transpose mat)))
        v4 (transpose (mapv (comp vec reverse row-op reverse) (transpose mat)))]
    (reduce (partial mat-op acc-op) [v1 v2 v3 v4])))

(defn solve1 [input]
    (->> (compute-visibility input is-visible #(or %1 %2))
         flatten
         (filter true?)
         count))
        
(defn solve2 [input]
    (->> (compute-visibility input num-visible *)
         flatten
         (reduce max)))

(defn -main []
  (let [input (parse-input "resources/input08.txt")]
    (println (solve1 input))
    (println (solve2 input))))
