(ns aoc.day20
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (mapv parse-long)))

(defn move-values [values positions ind]
  (let [num-values (count values)
        old-ind (positions ind)
        new-ind (mod (+ old-ind (values ind)) (dec num-values))]
    (-> positions
        (update-vals #(cond
                        (<= old-ind % new-ind) (dec %)
                        (<= new-ind % old-ind) (inc %)
                        :else %))
        (assoc ind new-ind))))

(defn solve [values]
  (let [positions
        (vec (range (count values)))
        new-positions
        (reduce (partial move-values values) positions positions)
        new-values
        (apply assoc (cons values (interleave new-positions values)))]
    (->> new-values
         cycle
         (drop-while (partial not= 0))
         (take-nth 1000)
         (take 4)
         (apply +))))

(defn -main []
  (let [vals (parse-input "resources/input20.txt")]
    (println (solve vals))))
