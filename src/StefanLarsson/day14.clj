(ns StefanLarsson.day14
  (:gen-class)
  (:require [clojure.string :as string]))


(defn line-parse [s]
  (let [
    m (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" s)
    [px py vx vy]  (mapv read-string (drop 1 m))]
    {:pos [px py] :v [vx vy]} ))

(defn file-parse [fname]
  (->> fname 
    slurp
    string/split-lines
    (map line-parse)))

(defn build-state [fname w h]
  {:w w :h h :robots (file-parse fname)})

; Need a function that takes two numbers and adds them modulo another number
(defn f [m x y]
  (mod (+ x y) m))

(defn ff [m]
  (partial f m))

; That's not really what I need
; We have two different moduli
; px py vx vy
; want (px + vx) mod w and (py + vy) mod h)
(defn mod-add [ u v m]
  (let [
    temp (mod (+ u v) m)]
    (if (< temp 0) (+ temp m) temp)))
; Actually we want n steps:
; (px + n * vx) mod w 
; (py + n * vy) mod h 

(defn step-dir [n m pos v]
  (let [temp (mod (+ (* n v) pos) m)]
    (if (< temp 0) (+ temp m) temp)))

(defn step-robot-n [robot w h n]
  (let [
    f (partial step-dir n)
    new-x (step-dir n w ( (robot :pos) 0) ((robot :v) 0))
    new-y (step-dir n h ( (robot :pos) 1) ((robot :v) 1))
    ]
  ;(assoc robot :pos (mapv  f [w h]  (robot :pos) (robot :v)))
  (assoc robot :pos [new-x new-y])
  )
)
(defn robots-step-n [robots w h n]
  (map #(step-robot-n % w h n) robots))
  
;; Safety factor?
;; How to determine quadrant?
;; suppose width = 5
;; 01234
;; The possible coordinates are above
;; 01 are in first part 34 in second, 3 in none
;; it is in the firt part if x is closer to 0 than to 4
;; distance to 0 is x
;; distance to 4 is (4 - x)
;; So in first part if x < 4-x (same as 2x < 4))
;; in second part if  x > 4 - x (same as 2x > l)
;; in no part if x = 4 - x (same 2x = 4))
;; we compare 2x + 1 with w, 2y +1 with h

;; intervals:
;; 0..mid mid mid+1... w-1 
;; correspond to 0 1 2?

(defn half [x w]
  (let [compx (inc (* 2 x))]
    (cond
      (< compx w) 0
      (= compx w) 1
      :else 2
)))

(defn quadrant [[ x y] w h]
  [(half x w) (half y h)] )

  
(defn soln [fname w h n]
  (let [
    initial-state (build-state fname w h)
    initial-robots (:robots initial-state)]
 
;;ipos (map :pos (:robots  (build-state fname w h)))]
    
  (->> initial-robots
    (map #(step-robot-n % w h n))
    (map :pos)
    (map #(quadrant % w h))
    (filter (fn [[hx hy]] (not (or (= 1 hx ) (= 1 hy)))))
    frequencies
    vals
    (apply *)
 ))) 

(defn display-line [positions y w]
  (do
    (print "|")
    (doseq 
            [x (range w)]
              (if (positions [x y]) ( print "*")
                ( print " ")))
    (println "|")))

(defn display-positions [positions w h]
  (do 
    (println (apply str (repeat (+ 2 w) \+)))
    (doseq [y (range h)]
      (display-line positions y w))
    (println (apply str (repeat (+ 2 w) \+)))
  )
)

(defn display-sequential-positions [fname w h start end]
  (let [initial-robots (:robots (build-state fname w h))]
  (loop [robots (map  #(step-robot-n % w h start) initial-robots) i start ]
    (if (= end i) nil
      (do
        (println (str "i = " i))
        (display-positions (into #{} (map :pos robots)) w h)
        (recur (map #(step-robot-n % w h 1) robots) (inc i))
        )))))

(defn part2 []
  (display-sequential-positions "resources/day14.txt" 101 103 8040 8060
  ))
