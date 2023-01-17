(ns aoc.day21
  (:require [clojure.string :as str]))

(defn str-to-op [s]
  (if (= s "/")
    quot
    (eval (symbol s))))

(defn parse-line [line]
  (let [[target left op right] (str/split line #"[ :]+")]
    (if (re-matches #"\d+" left)
      [target (parse-long left)]
      [target [(str-to-op op) left right]])))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)
       (into {})))

(defn compute [operations target]
  (let [rule (operations target)]
    (if (number? rule)
      rule
      (let [[op & args] rule]
        (apply op (map (partial compute operations) args))))))

(def associative-op #{+ *})
(def inverse-op {+ -, - +, * quot, quot *})

(defn compute2 [operations target]
  "If the result doesn't depend on 'humn' a number is returned,
  otherwise it's a function that takes the desired output,
  and returns the required value of 'humn' to get to that output."
  (let [rule (operations target)]
    (cond (= target "humn") identity
          (number? rule) rule
          :else
          (let [[op & args] rule
                [a b :as args] (mapv (partial compute2 operations) args)]
            (cond
              (not-any? fn? args)
              (apply op args),
              (= target "root")
              (if (fn? a) (a b) (b a)),
              (fn? a)
              (comp a #((inverse-op op) % b)),
              (associative-op op)
              (comp b #((inverse-op op) % a)),
              :else
              (comp b #(op a %)))))))

(defn solve [operations]
  (compute operations "root"))

(defn solve2 [operations]
  (compute2 operations "root"))

(defn -main []
  (let [operations (parse-input "resources/input21.txt")]
    (doseq [solve-fn [solve solve2]]
      (println (solve-fn operations)))))
