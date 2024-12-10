(ns StefanLarsson.aoc
	(:require [
    clojure.string :as string ])
  (:require [ clojure.set :as set])
  (:require [StefanLarsson.utils :as utils])
  (:require [StefanLarsson.day1 :as day1])
  (:require [StefanLarsson.day2 :as day2])
  (:require [StefanLarsson.day3 :as day3])
  (:require [StefanLarsson.day4 :as day4])
  (:require [StefanLarsson.day6 :as day6])
  (:require [StefanLarsson.day7 :as day7])
  (:require [StefanLarsson.day8 :as day8])
  (:require [StefanLarsson.day9 :as day9])
  (:gen-class))

;; Day 10 - Hoof It

(defn parse-topo-map [lines]
  (let [
    h (count lines)
    w (count (lines 0))
  ]
  { :w w :h h :lines lines}))

(defn height-at [[x y] topo-map]
  (let [
    c (.charAt ((topo-map :lines) y) x)
  ]
  (read-string (str c))) 
  )

(defn positions-at-height [h topo-map]
  (filter #(= h (height-at % topo-map))
    (for [
      y (range (topo-map :h))
      x (range (topo-map :w))]
      [x y])))

(defn nines [topo-map]
  (positions-at-height 9 topo-map))


(def topo-dirs [[-1 0] [1 0] [0 -1] [0 1]])

(defn on-topo-map? [[x y] topo-map]
  (and 
    (< -1 x (topo-map :w))
    (< -1 y (topo-map :h))))

(defn neighbours [pos topo-map]
  (let [
    candidates (map #( apply vector %) (map #(map + pos %) topo-dirs))]
  (filter #(on-topo-map? % topo-map) candidates)))

(defn sum-of-neighbouring-multiplicities [ pos topo-map multiplicities-map]
  (let [ n (neighbours pos topo-map)
    multiplicities (filter identity (map multiplicities-map n))]
    (apply + multiplicities)))

  
(defn eights-with-multiplicities [topo-map]
  (let [
    nines (nines topo-map)
    nines-with-1 (into {} (map #(vector % 1) nines))
    eights (positions-at-height 8 topo-map)
    eights-with-neighbours (map #(vector % (neighbours % topo-map)) eights)]
  (into {} (map #(vector % ( sum-of-neighbouring-multiplicities % topo-map nines-with-1)) eights))))

(defn show-map [topo-map multiplicities-map]
  (doall (do
    (println "++++++++++++++++++++++++++++++++")
    (for [y (range (topo-map :h))]
      (do
        (doall (for [x (range (topo-map :w))]
          (print (str "|" (or ( multiplicities-map [x y]) \.)) )))
        (println "|")
        (println "++++++++++++++++++++++++++++++++++")
      )))))
      
    
(defn xxxx [topo-map]
  (let [nines-with-1 (into {} (map #(vector % 1) (nines topo-map)))]
  (loop [current-value 9 multiplicities-map nines-with-1 ]
    ;(println "AAA" current-value multiplicities-map)
    ;(println "current-value: " current-value)
    ;(show-map topo-map multiplicities-map)
    (if (= 0 current-value) multiplicities-map
        (let
          [ u (positions-at-height (dec current-value) topo-map)
          v ( map #(vector % (sum-of-neighbouring-multiplicities % topo-map multiplicities-map)) u)]
        (recur (dec current-value) (into {} v)))))))

(defn yyy [topo-map]
  (let [nines-with-selfmap (into {} (map #(vector % (set (vector %))) (nines topo-map)))
      ]
    nines-with-selfmap
  (loop [current-value 9 reachable-nines-map nines-with-selfmap ]
    ;(println "AAA" current-value reachable-nines-map)
    ;(println "current-value: " current-value)
    ;(show-map topo-map multiplicities-map)
    (if (= 0 current-value) reachable-nines-map
        (let
          [ u (positions-at-height (dec current-value) topo-map)
         ; v ( map #(vector % (sum-of-neighbouring-multiplicities % topo-map multiplicities-map)) u)
          ;v (map #(vector % (map reachable-nines-map ( neighbours % topo-map))) u) 
          v (map #(vector % (apply set/union (map reachable-nines-map ( neighbours % topo-map)))) u) 
          ]
        ;(println "CCC" u)
        ;(println "BBB" v)
        (recur (dec current-value) (into {} v)))))))

(defn day10_1 []
  (let [
    topo-map
    (-> "resources/day10.txt"
      utils/file-to-lines
      parse-topo-map
    )
    ;thething (xxx topo-map)
    thething (yyy topo-map)
  ]
  ;(println topo-map)
  ;(println (into {} (map #(vector % 1) (nines topo-map))))
  ;(println (neighbours [0 0] topo-map)))
  
  ;(println thething)  
  ;(println (apply + ( vals thething)))
  ;(apply + ( vals thething))
  (apply + (map count ( vals thething)))
))
  

(defn day10_2 []
  (let [
    topo-map
    (-> "resources/day10.txt"
      utils/file-to-lines
      parse-topo-map
    )
    thething (xxxx topo-map)
    ;thething (yyy topo-map)
  ]
  ;(println topo-map)
  ;(println (into {} (map #(vector % 1) (nines topo-map))))
  ;(println (neighbours [0 0] topo-map)))
  
  ;(println thething)  
  ;(println (apply + ( vals thething)))
  (apply + ( vals thething))
  ;(apply + (map count ( vals thething)))
))

(defn input-filename [day use-example]
  (string/join (concat ["resources/"] (if (use-example) ["example_"] []) ["day"] [(str day) ".txt"])))

(def days {
  2 [day2/day2_1 day2/day2_2]
  3 [day3/day3_1 day3/day3_2]
  4 [day4/day4_1 day4/day4_2]
  6 [day6/day6_1 day6/day6_2]
  7 [day7/day7_1 day7/day7_2]
 ; 8 [day8/day8_1 ]}
  9 [day9/day9_1 day9/day9_2 ]
  10 [day10_1 day10_2]
  }
)

(defn all
  "Do all the things we have solutions for"
  []
  (do
    (day1/day1_2)
    (dorun (for [day (keys days)]
      (do
        (println "Day " day "results:")
        (dorun (for [function (days day)]
          (println (apply function '())))))))))


(defn single-day [data]
  (let [day (:day data)]
    (do
      (println "Day " day)
      (let [dayfns (days day)]
        (println "Results:"  (map #(apply % '()) dayfns))))))


 (defn -main

  "I don't do a whole lot ... yet."
  [& args]
  (all  ))

