(ns aoc.day21
  (:require [clojure.string :as str]))

(defn str-to-op [s]
  (get {"/" quot} s (eval (symbol s))))

(defn parse-line [line]
  (let [[target left op right] (str/split line #"[ :]+" )]
    (if (re-matches #"\d+" left)
      [target (parse-long left)]
      [target [(str-to-op op) left right]])))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)
       (into {})))

(def compute
  (memoize (fn [operations target]
             (let [rule (operations target)]
               (if (int? rule)
                 rule
                 (let [[op & args] rule]
                   (apply op (map (partial compute operations) args))))))))

(defn solve [operations]
  (compute operations "root"))

(defn -main []
  (let [operations (parse-input "resources/input21.txt")]
    (println (solve operations))))
