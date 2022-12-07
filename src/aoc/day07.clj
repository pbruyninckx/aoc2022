(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines))

(defn get-dir [cd-cmd]
  (subs cd-cmd 5))

; The state
; {:location ["dir" "subir"] ; works as a stack
;  :tree     {:files {"filename" size}
;             :dirs  <subtree>       }}

(defn get-tree-path [state]
  "Transform a state of the form [dir] into the full path in the state tree,
   e.g. [:tree :dir dir :subdir]"
  (cons :tree (interleave (repeat :dirs)
                          (:location state))))


(defn create-dir [{:keys [tree location] :as state} new-dir]
  (update-in state
             (concat (get-tree-path state) [:dirs new-dir])
             (fnil identity {})))

(defn add-file [state file-line]
  (let [[size file] (str/split file-line #" ")
        size (Integer/parseInt size)]
    (assoc-in state
              (concat (get-tree-path state) [:files file])
              size)))

(defn process-line [{:keys [tree location] :as state} ^String line]
  (cond
    (= line "$ cd /")
    (assoc state :location [])
    (= line "$ ls")
    state
    (= line "$ cd ..")
    (update state :location pop)
    (str/starts-with? line "$ cd ")
    (update state :location #(conj % (get-dir line)))
    (str/starts-with? line "dir")
    (create-dir state (subs line 4))
    :else ; File and file size
    (add-file state line)))

(defn add-own-size [node]
  (if-let [files (:files node)]
    (assoc node :size (reduce + (vals files)))
    (if
      (:dirs node)
      (assoc node :size 0)
      node)))

(defn add-size [node]
  (if-let [size (:size node)]
    (assoc node :tree-size
                (reduce + size (map :tree-size (vals (:dirs node)))))
    node))

(defn flatten-sizes [node]
  (if-let [size (:tree-size node)]
    (conj (flatten (vals (:dirs node))) size)
    node))


(defn get-sizes [input]
  (->> (reduce process-line
          {:tree {}
           :location []}
          input)
       (postwalk add-own-size)
       (postwalk add-size)
       (postwalk flatten-sizes)
       :tree))

(def MAX-SIZE 100000)
(defn solve1 [sizes]
  (->> sizes
       (filter #(< % MAX-SIZE))
       (reduce +)))

(def TO-FREE 30000000)
(def DISK-SIZE 70000000)
(defn solve2 [sizes]
  (let [total-size (first sizes)
        unused (- DISK-SIZE total-size)
        needed (- TO-FREE unused)]
    (->> sizes
         (filter #(>= % needed))
         (reduce min))))

(defn -main []
  (let [input (parse-input "resources/input07.txt")
        sizes (get-sizes input)]
    (println (solve1 sizes))
    (println (solve2 sizes))))
