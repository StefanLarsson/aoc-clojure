(ns StefanLarsson.day12
  (:require [StefanLarsson.utils :as utils])
  (:gen-class))

(defn build-map [filename]
  (let [
    lines (utils/file-to-lines filename)
    h (count lines)
    w (count (lines 0))
  ]
  { :lines lines :w w :h h})
)

(def directions [[ 0 -1] [ -1 0] [ 1 0] [0 1]])

(defn on-board [[x y] map]
  (and
    ( < -1 x (map :w))
    ( < -1 y (map :h))  ))

(defn neighbours [pos board]
  (as-> directions v
  (map  #(into []  (map + pos %)) v)
  ;(map #(vector (map  + pos %)) v) 
  (filter #(on-board % board) v)
))
(defn plot-type [[x y] board]
  (.charAt ( (board :lines) y) x ))


(defn plot [pos board]
  (let [pt (plot-type pos board)]
    (loop [active (list pos)
          visited #{}]
      ;(println "active: " active)
      ;(println "visited: " visited)
      (if (empty? active) visited
        (let [p (first active)
              neighbours (neighbours p board)
              similar-neighbours (filter #(= pt (plot-type % board)) neighbours)
              not-visited-neighbours (filter (complement visited) similar-neighbours)
              active-2 (into (rest active) not-visited-neighbours)]
          ;(println "p: " p)
          ;(println "rest active " (rest active) )
          ( if (visited p)
            (recur active-2 visited)
            (recur active-2 (conj visited p ))
        ))))
))

(defn in-some-plot [pos plots]
  ;(println pos)
  ;(println plots)
  (some identity (map #(% pos) plots)))
(defn all-plots [board]
  (let [
    all-pos (for
      [x (range (board :w) ) y (range (board :h))]
      [x y])
    ]
  (reduce #(if ( in-some-plot %2 %1) %1 (conj %1 (plot  %2 board))) #{}    all-pos )  
  )
)

(defn potential-neighbours [pos]
  (as-> directions v
  (map  #(into []  (map + pos %)) v)
))

(defn perimeter [plot]
  (->> plot
    (map potential-neighbours)
    (map #(filter (complement plot) %))
    (map count)
    (apply +)))

(defn day12_1
  ([filename]
    (let [board (build-map filename)
        plots (all-plots board)]
    (apply +  (map #(* ( perimeter %) (count %)) plots))
      
    )
    )
  )

