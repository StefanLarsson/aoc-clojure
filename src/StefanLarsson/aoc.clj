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

; Just copied from REPL
(defn day1_2 []

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
;  (println l1 l2)
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

(defn drop-nth [n seq] (loop [n n left '() x (first seq) xs (rest seq)] (cond (= 1 n) (concat left xs) (empty? xs) (concat left (list x)) :else  (recur (dec n) (concat left (list x)) (first xs) (rest xs)))))

(defn dropped [seq] (map #(drop-nth % seq) (map inc (range (count seq)))))

(defn day2_1 []
  (let [lines (file-to-lines "resources/day2.txt")]
    (->> lines
      (map line-to-report )
      (filter report-safe?)
      count)))

(defn day2_2 []
  (let [lines (file-to-lines "resources/day2.txt")]
    (->> lines
      (map line-to-report )
      (map #(conj ( dropped %) %))
      (filter #(some report-safe? %))
      count)))

(defn day3_1 [] 
  (let [text (slurp "resources/day3.txt")
        matches (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" text)]
  (->>  matches
    (map #(drop 1 %))
    (map #(map read-string  %))
    (map #( apply * %))
    (apply +))))


(defn f [state val]
  (cond
    (= (val 0) "do()") (assoc state :enabled true)
    (= (val 0) "don't()") (assoc state :enabled false)
    (state :enabled) (assoc state :sum (+ (state :sum) (* (read-string (val 1)) (read-string (val 2)))))
    :else state))

(defn day3_2 [] 
  (let [text (slurp "resources/day3.txt")
        matches (re-seq #"do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)" text)]
(reduce f {:enabled true :sum 0 } matches)
    ))

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
    lines ( file-to-lines "resources/day4.txt")
    side (count lines)
    all-strings (map #(string-from-positions % lines) (all-lines side)) ; just assume square for now
    all-matches (map #(re-seq #"(?=(XMAS|SAMX))" %) all-strings)]
    ;(vertical-at-pos 4 side)
    ;(diagonal-down-right-at-pos 4 side)
    ;(diagonal-down-left-at-pos 144 side)
    ;(horizontal-at-pos 144 side)
    ;(all-lines 3)
    ;(char-at-pos 0 0 lines)
    ;(string-from-positions '([0 0] [0 1]) lines)
    ;(map #(string-from-positions % lines) (all-lines side))
    ;(count  all-matches)
    (apply +  (map #(count (re-seq #"(?=(XMAS|SAMX))" %) ) all-strings))
))
(defn all
  "Do all the things we have solutions for"
  []
  (do
    (day1_2)
    (println (day2_1))
    (println (day2_2))
    (println (day3_1))
    (println (day3_2))
    (println (day4_1))
))


 (defn -main

  "I don't do a whole lot ... yet."
  [& args]
  (all  ))

