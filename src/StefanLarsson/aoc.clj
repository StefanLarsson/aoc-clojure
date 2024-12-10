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
  (:require [StefanLarsson.day10 :as day10])
  (:gen-class))


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
  10 [day10/day10_1 day10/day10_2]
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

