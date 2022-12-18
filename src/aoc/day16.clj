(ns aoc.day16
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn parse-line [line]
  (let [[valve flow & tunnels] (re-seq #"[A-Z]{2}|\d+" line)]
    {valve {:flow (parse-long flow)
            :tunnels (vec (sort tunnels))}}))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)
       (reduce merge)))

(defn update-released [{:keys [flow] :as state}]
  (update state :released (partial + flow)))

(defn next-states [pipes {:keys [flow released location open] :as state}]
  (concat
    (if (and (not (open location)) (pos? (get-in pipes [location :flow])))
      [(-> state
           update-released
           (update :flow (partial + (get-in pipes [location :flow])))
           (update :open #(conj % location)))
       ])
    (->> (get-in pipes [location :tunnels])
         (map (fn [new-location]
                (-> state
                    update-released
                    (assoc :location new-location)))))))

(defn strip-flow [state]
  (dissoc state :flow :released))

(defn iterate-states [pipes [seen states]]
  (let [next-states (->> (apply concat (map (partial next-states pipes) states))
                         (filter #(not (seen (strip-flow %)))))
        next-seen (reduce conj seen (map strip-flow next-states))]
    [next-seen next-states]))



(defn solve [pipes]
  (let [start-state {:flow 0
                     :released 0
                     :location "AA"
                     :open #{}}]
    (->> (iterate (partial iterate-states pipes) [#{} [start-state]])
         (drop 30)
         (map second)
         first
         (map :released)
         (sort >)
         first)))

(defn -main []
  (let [input (parse-input "resources/input16.txt")]
    (pp/pprint (solve input))))
