(ns aoc.day17
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn parse-input [f]
  (->> f
       slurp
       str/trim))

(def blocks
  "Each line is represented as an int, leftmost = 1, 2, 4, 8, ..."
  (cycle
    [[(+ 1 2 4 8)]
     [     2
      (+ 1 2 4)
           2   ]
     [(+ 1 2 4)
             4
             4]
     [1 1 1 1]
     [3 3]]))


(defn overlaps? [tower block [h x]]
  (->> block
       (map-indexed vector)
       (filter (fn [[i b-line]]
                 (let [b-line (bit-shift-left b-line x)]
                   (or (< h 0)
                       (< x 0)
                       (not= 0 (bit-and b-line
                                        (get tower (+ i h) 0)))
                       (not= 0 (bit-and b-line (bit-shift-left 1 7)))))))
       not-empty
       boolean))

(defn add-to-tower [tower block [h x]]
  (reduce
     (fn [tower [i b-line]]
       (update tower
               (+ i h)
               (fnil #(bit-or % (bit-shift-left b-line x)) 0)))
     tower
     (map-indexed vector block)))

(defn lower [pos]
  (update pos 0 dec))

(defn push [direction pos]
  (update pos 1 ({\< dec, \> inc} direction)))

(defn get-tower-top [tower]
  (if (> (count tower) 10)
    (subvec tower (- (count tower) 10))))

(defn solve [jet-patterns target-blocks]
  (loop [[block & _ :as blocks] blocks
         block-position [3 2]
         [[ind-dir direction] & _ :as jet-patterns] jet-patterns
         tower []
         num-fallen 0
         seen-tops {}]
    (cond
      (= target-blocks num-fallen)
      (count tower)
      :else
      (let [suggested-position (push direction block-position)
            block-position (if (overlaps? tower block suggested-position)
                             block-position
                             suggested-position)]
        (if
          (overlaps? tower block (lower block-position))
          (let [new-tower (add-to-tower tower block block-position)]
            (let [[old-height old-num-fallen]
                  (seen-tops [(get-tower-top new-tower) ind-dir (second blocks)])]
              (if (and old-height
                       (= 0 (rem (- target-blocks old-num-fallen) (- (inc num-fallen) old-num-fallen))))
                (+ old-height (* (- (count new-tower) old-height)
                                 (quot (- target-blocks old-num-fallen)
                                       (- (inc num-fallen) old-num-fallen))))
                (recur
                  (rest blocks)
                  [(+ (count new-tower) 3) 2]
                  (rest jet-patterns)
                  new-tower
                  (inc num-fallen)
                  (conj seen-tops [[(get-tower-top new-tower) ind-dir (second blocks)]
                                   [(count new-tower) (inc num-fallen)]])))))
          (recur
            blocks
            (lower block-position)
            (rest jet-patterns)
            tower
            num-fallen
            seen-tops))))))

(defn -main []
  (let [jet-patterns (->> (parse-input "resources/input17.txt")
                          (map-indexed vector)
                          cycle)]
    (doseq [target-blocks [2022 1000000000000]]
      (println (solve jet-patterns target-blocks)))))