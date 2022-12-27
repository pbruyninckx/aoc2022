(ns aoc.day19
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.set :as set]))

(defn parse-blueprint [s]
  "Prices are represented as {<ore kind> <price>}"
  (let [[id & prices] (map parse-long (re-seq #"\d+" s))
        prices (vec prices)]
    {:id id
     :prices [{0 (prices 0)} {0 (prices 1)} {0 (prices 2) 1 (prices 3)} {0 (prices 4) 2 (prices 5)}]}))

(defn parse-inputs [f]
  (->> f
       slurp
       str/split-lines
       (map parse-blueprint)))

(def start-state
  {:robots [1 0 0 0]
   :ore [0 0 0 0]})

(defn pay-price [ore price]
  (reduce (fn [ore [ore-kind num-ore]] (update ore ore-kind #(- % num-ore)))
          ore
          price))

(defn mine-ore [robots]
  (fn [state]
    (->> (map-indexed vector robots)
         (reduce (fn [state [robot-ind robot-num]]
                   (update-in state [:ore robot-ind] (partial + robot-num)))
                 state))))

(defn next-states [prices {:keys [robots ore] :as state}]
  (let [all-states
        (for [robot (range 4)]
          (-> state
              (update-in [:robots robot] inc)
              (update :ore #(pay-price % (prices robot)))))
        valid-states (filter (fn [{:keys [ore]}] (every? (complement neg?) ore)) all-states)
        new-ore-fn (mine-ore robots)]
    (->> (cons state valid-states)
         (map new-ore-fn)
         set)))

(defn keep-best-states [states]
  (->> states
       (sort-by #(vec (reverse (interleave (:ore %) (:robots %)))) (comp - compare))
       (take 200)))

(defn next-minute [prices states]
  (->> states
       (map (partial next-states prices))
       (reduce set/union)
       keep-best-states))

(defn max-geodes [num-iterations blueprint]
  (->> #{start-state}
       (iterate (partial next-minute (:prices blueprint)))
       (drop num-iterations)
       first
       (map #(get-in % [:ore 3]))
       (reduce max)))

(defn solve1 [blueprints]
  (->> blueprints
       (map (fn [bp] (* (max-geodes 24 bp) (:id bp))))
       (reduce +)))

(defn solve2 [blueprints]
  (->> blueprints
       (take 3)
       (map (partial max-geodes 32))
       (apply *)))

(defn -main []
  (let [blueprints (parse-inputs "resources/input19.txt")]
    (doseq [solve [solve1 solve2]]
      (println (solve blueprints)))))
