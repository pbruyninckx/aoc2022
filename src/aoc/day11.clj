(ns aoc.day11
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

(defn parse-operation [line]
  (let [[a op b] (drop 4 (str/split line #" +"))]
    (fn [x]
      (letfn [(substitute-old [arg]
                (if (= "old" arg) x (Integer/parseInt arg)))]
        (apply (resolve (symbol op)) (map substitute-old [a b]))))))

(defn parse-destination [destination-lines]
  (let [[divisor dest-true dest-false]
        (->> destination-lines
             (map #(re-find #"\d+" %))
             (map #(Integer/parseInt %)))]
    (fn [x] (if (= 0 (rem x divisor)) dest-true dest-false))))

(defn parse-monkey [modulo-factor [_ items op & destination]]
  "{:items       ; worry level of items (fifo queue)
    :operation   ; function the creates new worry-level
    :destination ; function that returns destination of item with worry-level }"
  {:items (queue (map #(Integer/parseInt %) (re-seq #"\d+" items)))
   :operation (comp #(mod % modulo-factor) (parse-operation op))
   :destination (parse-destination destination)
   :inspections 0})

(defn stress-reducer [monkey]
  "Reduces the amount of stress after update by a third"
  (update monkey :operation (partial comp #(quot % 3))))

(defn process-monkey [monkeys monkey-id]
  (let [monkey (monkeys monkey-id)
        {:keys [operation destination]} monkey]
    (loop [monkeys monkeys]
      (if-let [item (peek (get-in monkeys [monkey-id :items]))]
        (let [new-item (operation item)]
          (recur
            (-> monkeys
                (update-in [(destination new-item) :items]
                           #(conj % new-item))
                (update-in [monkey-id :items]
                           pop)
                (update-in [monkey-id :inspections] inc))))
        monkeys))))

(defn count-inspections [monkeys]
  (->> monkeys
       (mapv :inspections)))

(defn process-round [monkeys]
  (reduce process-monkey monkeys (range (count monkeys))))

(defn parse-modulo-factor [file-data]
  (->> file-data
       (re-seq #"by (\d+)")
       (map (comp #(Integer/parseInt %) second))
       (apply *)
       (* 3)))

(defn parse-input [file-name]
  (let [file-data (slurp file-name)
        modulo-factor (parse-modulo-factor file-data)]
    (->> file-data
         str/split-lines
         (partition-by empty?)
         (take-nth 2)
         (mapv (partial parse-monkey modulo-factor)))))

(defn solve [part monkeys]
  (let [[stress-reducer num-iterations]
        (if (= part 1)
          [stress-reducer 20]
          [identity 10000])]
    (->> monkeys
         (mapv stress-reducer)
         (iterate process-round)
         (map count-inspections)
         (drop num-iterations)
         first
         (sort >)
         (take 2)
         (apply *))))

(defn -main []
  (let [monkeys (parse-input "resources/input11.txt")]
    (dorun (map #(println (solve % monkeys)) [1 2]))))
