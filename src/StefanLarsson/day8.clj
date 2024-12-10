(ns StefanLarsson.day8
  (:require [clojure.string :as string])
  (:gen-class))

         
;; Day 8: Resonant Collinearity
  
;; Key info;
;; Puzzle input is map of antennas
;; Antinodes: points twice as far from one antenna as from the other
;; ....#.a.a.#....
;; ..#..b..b..#...
;; ...#cc#...
;; Antennae have frequencies
;; lowercase letter, uppercase letter, digit
;; Confused description! What about the harmonically conjugate points?
;; input consists of characters, then a newline, then more characters,...
(defn string-to-map2d [s]
  (let [width (string/index-of s \newline) l (count s)]
  { :mapstring s :width width  :height (/ l (inc width))})
)

(defn on-map? [[x y] map2d]
  (and
    (< -1 x (map2d :width)) (-1 y (map2d :height))
  )
)


(defn value-at [map2d [x y]]
  (.charAt (map2d :mapstring) (+ x  (* y (inc (map2d :width) )))))
; I want to find all pairs of antennae of same frequency 

(defn all-positions [map2d]
  (for [x (range (map2d :width))  y (range (map2d :height))]
    [x y]))

(defn map2d-group-by-value [map2d]
  (dissoc (group-by #(value-at map2d %)  (all-positions map2d)) \.))

(defn map2d-frequencies [map2d]
  (frequencies (map #(value-at map2d %) (all-positions map2d))))

; .....A...A........
; .#...A...A...#....
;      x1  x2
;  (x1 - (x2 - x1) = 2x1 - x2
;              x2 + (x2 - x1) = 2x2 - x1 
; this is the case where x2 > x1 !
; otherwise we will be at x1 + (x1 - x2) and x2 - (x1 -x2) which will be 
; the same two point just reordered!

(defn anti-nodes [[x1 y1] [x2 y2]]
  (let [dx1 (* 2 x1) dx2 (* 2 x2) dy1 (* 2 y1) dy2 (* 2 y2)]
    (vector [ (- dx1 x2) (- dy1 y2) ] [ (- dx2 x1) (- dy2 y1)])))

(defn day8_1 [] 
  (let [
    input (slurp "resources/day8.txt")
    map2d (string-to-map2d input)
    value-pos-map (map2d-group-by-value map2d)
  ]
  value-pos-map))
