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
        (quot (apply (resolve (symbol op)) (map substitute-old [a b]))
              3))))) ; division by 3 is included here

(defn parse-destination [destination-lines]
  (let [[divisor dest-true dest-false]
        (->> destination-lines
             (map #(re-find #"\d+" %))
             (map #(Integer/parseInt %)))]
    (fn [x] (if (= 0 (rem x divisor)) dest-true dest-false))))

(defn parse-monkey [[_ items op & destination]]
  "{:items       ; worry level of items (fifo queue)
    :operation   ; function the creates new worry-level
    :destination ; function that returns destination of item with worry-level }"
  {:items (queue (map #(Integer/parseInt %) (re-seq #"\d+" items)))
   :operation (parse-operation op)
   :destination (parse-destination destination)
   :inspections 0})



(defn process-monkey [monkeys monkey-id]
  (let [monkey (monkeys monkey-id)
        {:keys [operation destination]} monkey]
    (loop [monkeys monkeys]
      ;(println (seq (get-in monkeys [monkey-id :items])))
      (if-let [item (peek (get-in monkeys [monkey-id :items]))]
        (let [new-item (operation item)]
          ;(println item " as " new-item " from " monkey-id " to " (destination new-item))
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

(defn parse-input [file-name]
  (->> file-name
       slurp
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (mapv parse-monkey)))

(defn solve [monkeys]
  (->> monkeys
       (iterate process-round)
       (map count-inspections)
       (drop 20)
       first
       (sort >)
       (take 2)
       (apply *)))

(defn -main []
  (let [monkeys (parse-input "resources/input11.txt")]
    (println (solve monkeys))))
