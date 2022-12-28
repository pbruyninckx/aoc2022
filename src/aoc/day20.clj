(ns aoc.day20
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (mapv parse-long)))

(defn move-values [values positions ind]
  "Move the value originally at position 'ind'.
  positions indicate the position in the index at which the value originally at that
  location currently appears."
  (let [num-values (count values)
        old-ind (positions ind)
        new-ind (mod (+ old-ind (values ind)) (dec num-values))]
    (-> positions
        (update-vals #(cond
                        (<= old-ind % new-ind) (dec %)
                        (<= new-ind % old-ind) (inc %)
                        :else %))
        (assoc ind new-ind))))

(def decryption-key 811589153)

(defn solve [part values]
  (let [values
        (if (= part 1) values (mapv (partial * decryption-key) values))
        positions
        (vec (range (count values)))
        new-positions
        (reduce (partial move-values values)
                positions
                (if (= part 1) positions (flatten (repeat 10 positions))))
        new-values  ; Put each value at the correct location in the vector
        (apply assoc (cons values (interleave new-positions values)))]
    (->> new-values
         cycle
         (drop-while (partial not= 0))
         (take-nth 1000)
         (take 4)
         (apply +))))

(defn -main []
  (let [vals (parse-input "resources/input20.txt")]
    (doseq [part [1 2]]
      (println (solve part vals)))))
