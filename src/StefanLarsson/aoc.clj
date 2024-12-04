(ns StefanLarsson.aoc
	(:require [clojure.string :as string])
  (:gen-class))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn file-to-lines
  "Read a file and split into lines"
  [filename]
  (->> filename
    slurp
    string/split-lines))


(defn day1_2 []

;(def lines (string/split-lines (slurp "resources/day1.txt")))
(def lines (file-to-lines "resources/day1.txt"))
(def lines-arrays (map #(string/split % #" +") lines))
(def lines-int-arrays (map #(map read-string %) lines-arrays ))
(def firsts (map first lines-int-arrays ))
(def lasts (map last lines-int-arrays ))
(def firsts-sorted (sort firsts))
(def lasts-sorted (sort lasts))
 (def x (map vector firsts-sorted lasts-sorted ))
(def diffs (map #(apply - %) x))
(def absdiffs (map abs diffs))
absdiffs
(apply + absdiffs )
(def firsts-vector (into [] firsts))
(filter #(.contains firsts-vector %) lasts)
(println  (apply + (filter #(.contains firsts-vector %) lasts))))

(defn line-to-report [s]
  (map read-string (string/split s #" +")))


(defn safe-diff? [d]
  (let [absdiff (abs d)]
    (and
      (<= 1 absdiff)
      (<= absdiff 3))))


(defn safe-step? [l1 l2]
  ;(println l1 l2)
  (let [absdiff (abs (- l1 l2))]
  (and (<= 1 absdiff) (<= absdiff 3))))

(defn report-safe-internal? [l1 l2 rls]
  (if
    (not (safe-step? l1 l2)) false
    ( loop [diff (- l1 l2) l l2 ls rls]
 ;   (println diff l ls)
      (cond
        (not (safe-step? l (first ls))) false
        (not (> (* diff (- l (first ls))) 0 )) false
        (empty? (rest ls) ) true
        :else (recur (- l (first ls)) (first ls) (rest ls)
        
          )))))

    
(defn report-safe?
  [levels]
 ; (println levels)
  (let [l1 (first levels) l2 (second levels) rls (rest (rest levels))]
    (cond
      (not l2) true ;; at most one level
      (empty? rls) (safe-step? l1 l2) ;; exactly two leves
      :else (report-safe-internal? l1 l2 rls))))

    
(defn day2_1 []
  (let [lines (file-to-lines "resources/day2.txt")]
    ;(println lines)
    (def reps (map line-to-report lines))
    ;(println reps)
    ;(println (map report-safe?  reps))
    (println (count (filter report-safe?  reps)))))

(defn all
  "Do all the things we have solutions for"
  []
  (do
    (day1_2)
    (day2_1)
))
 (defn -main

  "I don't do a whole lot ... yet."
  [& args]
  (all  ))

