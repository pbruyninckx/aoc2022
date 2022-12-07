(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]))

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

(def MAX-SIZE 100000)

(defn part1-walk [node]
  (if-let [size (:tree-size node)]
    (+ (if (<= size MAX-SIZE) size 0)
       (reduce + (vals (:dirs node))))
    node))

(defn solve [input]
  (->> (reduce process-line
          {:tree {}
           :location []}
          input)
       (postwalk add-own-size)
       (postwalk add-size)
       (postwalk part1-walk)
       :tree))

(defn -main []
  (let [input (parse-input "resources/input07.txt")]
    (println (solve input))))
