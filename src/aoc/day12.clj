(ns aoc.day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn flatten-1 [s]
  (apply concat s))

(defn parse-input [file-name]
  (let [heights
        (->> file-name
             slurp
             str/split-lines
             (mapv #(mapv int %)))
        positions (->> (map #(case (get-in heights %) 83 {:start %} 69 {:end %} nil)
                            (for [x (range (count heights)) y (range (count (first heights)))] [x y]))
                       (filter identity)
                       (into {}))]
    (into {:heights (-> heights
                        (assoc-in (:start positions) (int \a))
                        (assoc-in (:end positions) (int \z)))}
          positions)))

(defn get-neighbours [heights [x y :as pos]]
  (let [min-h (dec (get-in heights pos))]
    (->> [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
         (filter #(if-let [new-h (get-in heights %)]
                    (<= min-h new-h))))))


(defn solve [part {:keys [heights start end]}]
  (letfn [(start-reached? [current]
            (if (= part 1)
              (current start)
              (->> current
                   (filter #(= (get-in heights %) (int \a)))
                   not-empty)))
          (step [[seen current]]
            (let [neighbours (->> current
                                  (map #(get-neighbours heights %))
                                  flatten-1
                                  (into #{}))]
              [(set/union neighbours seen) (set/difference neighbours seen)]))]
    (->> (iterate step [#{end} #{end}])
         (map-indexed vector)
         (filter (fn [[_index [_seen current]]] (start-reached? current)))
         first
         first)))

(defn -main []
  (let [input (parse-input "resources/input12.txt")]
    (doall (map #(println (solve % input)) [1 2]))))
