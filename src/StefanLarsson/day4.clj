(ns StefanLarsson.day4
  (:require [StefanLarsson.utils :as utils])
  (:gen-class))


(defn vertical-at-pos [pos side]
  "Coordinates for vertical going down from (pos,0) to (pos, (side - 1))"
  (map #(vector pos %) (range side)))
;(defn vertical-at-pos [i ss]  (apply str (map #(.charAt (ss %) i)  (range (count ss)))))

(defn diagonal-down-right-at-pos [pos side]
  (let [
    r (range side)]
     (filter (fn [[x y]] (<= 0 x (dec side)) ) (map #(vector (+ pos %) %) r))))

(defn diagonal-down-left-at-pos [pos side]
  (let [
    r (range side)]
     (filter (fn [[x y]] (<= 0 x (dec side)) ) (map #(vector (- pos %) %) r))))

(defn horizontal-at-pos [pos side]
  (map #(vector % pos) (range side)))

(defn all-lines [side]
  (let [
    r1 (range side)
    horizontals (map #(horizontal-at-pos % side) r1)
    verticals (map #(vertical-at-pos % side) r1)
    r2 (range (- (* 2 side) 1))
    diagonals-left (map #(diagonal-down-left-at-pos % side) r2)
    r3 (range (-  1 side) side)
    diagonals-right (map #(diagonal-down-right-at-pos % side) r3)]
    (concat horizontals verticals diagonals-left diagonals-right)))

(defn char-at-pos [x y ss]
  (.charAt (ss y) x) )

(defn string-from-positions [poss ss]
  (apply str  (map #(char-at-pos (% 0) (% 1) ss) poss)))

(defn day4_1 [] 
  (let [
    lines ( utils/file-to-lines "resources/day4.txt")
    side (count lines)
    all-strings (map #(string-from-positions % lines) (all-lines side)) ; just assume square for now
    all-matches (map #(re-seq #"(?=(XMAS|SAMX))" %) all-strings)]
    (apply +  (map #(count (re-seq #"(?=(XMAS|SAMX))" %) ) all-strings))
))

(defn internal-positions [side]
  (for [i (range 1 (- side 1))
        j (range 1 (- side 1))]
      (vector i j)))

(defn is-center-MS2 [ x y ss]
  (and 
  (or 
    (and  (= \M (char-at-pos (- x 1) (- y 1) ss)) (= \S (char-at-pos (+ x 1) (+ y 1) ss)  ))
    (and  (= \S (char-at-pos (- x 1) (- y 1) ss)) (= \M (char-at-pos (+ x 1) (+ y 1) ss)  )))
  (or 
    (and  (= \M (char-at-pos (- x 1) (+ y 1) ss)) (= \S (char-at-pos (+ x 1) (- y 1) ss)  ))
    (and  (= \S (char-at-pos (- x 1) (+ y 1) ss)) (= \M (char-at-pos (+ x 1) (- y 1) ss)  )))))

(defn is-center-MS [x y ss]
  (and
    (or
      (and (= \M (char-at-pos (- x 1) (- y 1) ss) (= \S (char-at-pos (+ 1 x) (+ 1 y) ss))))
      (and (= \S (char-at-pos (- x 1) (- y 1) ss) (= \M (char-at-pos (+ 1 x) (+ 1 y) ss))))
    )
    ;(or
    ;  (and (= \M (char-at-pos (- x 1) (+ y 1) ss) (= \S (char-at-pos (+ 1 x) (- 1 y) ss))))
    ;  (and (= \S (char-at-pos (- x 1) (+ y 1) ss) (= \M (char-at-pos (+ 1 x) (- 1 y) ss))))
    ;i)
))
;; Find X-MAS:
;; The X-MASes correspond to the A:s in their centres.
;; We can find all the positions of A:s
;; And then verify they are center's om MAS in both diagonals
(defn day4_2 [] 
  (let [
    lines ( utils/file-to-lines "resources/day4.txt")
    side (count lines)
    a-positions (filter #(= \A ( char-at-pos ( % 0) (% 1) lines)) ( internal-positions side))]
    (count  (filter #(is-center-MS2 (% 0) (% 1) lines) a-positions))
    ;a-positions
))
