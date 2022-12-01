(ns aoc.core
  (:gen-class))

(defn -main
  "Runs the code for the given day"
  [day & _]
  (let [d (Integer/parseInt day)
        day-space (format "aoc.day%02d" d)]
    (require (symbol day-space))
    ((resolve (symbol day-space "run")))))
