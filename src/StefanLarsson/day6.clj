(ns StefanLarsson.day6
  (:require [clojure.string :as string])
  (:require [StefanLarsson.utils :as utils])
  (:gen-class))


; guard position is marked by ^ (we assume he is pointing north always)
(defn guard-coordinate-if-found [index line]
  (let [p (string/index-of line "^")]
    (if p (vector p index)
      nil)))

(defn find-guard-position [lines]
  "Find the position of the guard. Returns vector of x and y coordinates"
  (first  (keep-indexed guard-coordinate-if-found lines )))

(defn is-free? [[x y] lines]
  ;(println "CCC"  x y lines)
  ;(println (.charAt (lines y )x))
  (not (= \# (.charAt ( lines y) x))))

(defn on-board? [[x y] lines]
  (and 
    (< -1 x (count (lines 0)))
    (< -1 y (count lines))))

(defn next-position [[ x y] dir]
  (cond (= dir :up ) (vector x (dec y))
        (= dir :down ) (vector x (inc y))
        (= dir :left ) (vector (dec x) y)
        (= dir :right) (vector (inc x) y)))

(defn next-dir [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))

; No detection of infinite loops!


(defn next-state [[x y] dir on-board? is-free?]
  (loop [ [x y] [x y] dir dir]

  (let [try-pos (next-position [x y] dir)]
    (cond
      (not (on-board?  try-pos )) (vector try-pos dir )
      (is-free? try-pos)  (vector  try-pos dir )
      :else (recur [x y] (next-dir dir) )
  ))))

(defn calculate-visited-positions [[x y] dir lines]
  (let [h (count lines)
    w (count (lines 0))
    on-board? (fn [[x y]] (and (< -1 x w) (< -1 y  h) ))
    is-free?  (fn [[x y]] (not (= \# (.charAt (lines y) x))))]
  (loop [[x y] [x y] dir dir visited #{}]
    (let [stepped (next-state [x y] dir on-board? is-free?)]
    (if (not (on-board? (stepped 0) )) (conj  visited [x y])
      (recur (stepped 0) (stepped 1) (conj visited [x y])))))))

(defn day6_1 []
  (let [
    lines (utils/file-to-lines "resources/day6.txt")
    initial-position (find-guard-position lines)]
    (count  (calculate-visited-positions initial-position :up lines))
  ))

(defn is-cyclic? [[x y] dir lines [xo yo]]
  (let [
    h (count lines)
    w (count (lines 0))
    on-board? (fn [[x y]] (and (< -1 x w) (< -1 y h)))
    is-free? (fn [[x y]] (and (not (= \# (.charAt (lines y) x))) (not (= [x y] [xo yo]))))]
  (loop [[x y] [x y] dir dir seen-states #{}]
    (let [stepped (next-state [x y] dir on-board? is-free?)]
      (cond
        (not ( on-board? (stepped 0) )) false
        (contains? seen-states stepped) true
        :else (recur (stepped 0) (stepped 1) (conj seen-states [[x y] dir])))))))
 
(defn day6_2 []
  (let [
    lines (utils/file-to-lines "resources/day6.txt")
    initial-position (find-guard-position lines)
    potential-obstacles (filter #(not (= initial-position %)) (for [ xo (range (count lines)) yo (range (count (lines 0)))] [xo yo]))
    ]
    (count (filter #(is-cyclic? initial-position :up lines %) potential-obstacles))
    ;initial-position
    ;(is-cyclic? initial-position :up lines [3 6])
  ))
