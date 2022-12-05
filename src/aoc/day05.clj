(ns aoc.day05
  (:require [clojure.string :as str]))

(defn parse-action [^String s]
  (->> (str/split s #"\D+")
       rest
       (mapv #(Integer/parseInt %))
       ((fn [[a b c]] [a (dec b) (dec c)]))))

(defn parse-stacks [stack-input]
  (->> stack-input
       (apply map vector)
       (map (comp vec str/trim str/join rest reverse))
       (drop 1)
       (take-nth 4)
       vec))

(defn parse-input [f]
  (let [[stack-input [_ & actions]]
        (->> f
             slurp
             str/split-lines
             (split-with not-empty))]
    [(parse-stacks stack-input)
     (map parse-action actions)]))

(defn split-stack [stack top-n]
  (let [n (- (count stack) top-n)
        bottom-stack (subvec stack 0 n)
        top-stack (subvec stack n)]
    [bottom-stack top-stack]))

(defn move [stacks [num from to] transform-fn]
  (let [[new-stack moved-crates] (split-stack (stacks from) num)]
    (-> stacks
        (assoc from new-stack)
        (update to #(reduce conj % (transform-fn moved-crates))))))

(defn perform-actions [stacks actions transform-fn]
  (if (empty? actions)
    stacks
    (recur (move stacks (first actions) transform-fn)
           (rest actions)
           transform-fn)))

(defn solve [[stacks actions] transform-fn]
  (let [result (perform-actions stacks actions transform-fn)]
    (->> result
         (map last)
         (str/join))))

(defn -main []
  (let [input (parse-input "resources/input05.txt")]
    (println (solve input reverse))
    (println (solve input identity))))

