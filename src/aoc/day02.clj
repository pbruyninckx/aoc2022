(ns aoc.day02
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map (fn [line] (mapv #(get line %) [0 2])))))

(def winning-moves #{[\A \Y] [\B \Z] [\C \X]})
(def shape-score {\A 1 \B 2 \C 3 \X 1 \Y 2 \Z 3})


(defn score [[elf me :as turn]]
  (+ (cond 
       (= (shape-score elf) (shape-score me)) 3
       (winning-moves turn) 6
       :else 0)
     (shape-score me)))

(defn get-turn [[elf outcome]]
  [elf
   ((case outcome
     \X {\A \Z \B \X \C \Y}
     \Y {\A \X \B \Y \C \Z}
     \Z {\A \Y \B \Z \C \X})
    elf)])

(defn solve [part v]
  (->> v
       ((if (= part 1) identity #(map get-turn %)))
       (map score)
       (reduce +)))


(defn -main []
  (let [input (parse-input "resources/input02.txt")]
    (println (solve 1 input))
    (println (solve 2 input))))

