(ns StefanLarsson.day2
  (:require [clojure.string :as string])
  (:require [StefanLarsson.utils :as utils]))

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

(defn line-to-report [s]
  (map parse-long (string/split s #" +")))

(defn day2_1 []
  (->> "resources/day2.txt"
    (utils/parse-file line-to-report)
    (filter report-safe?)
    count))

(defn day2_2 []
  (->> "resources/day2.txt"
    (utils/parse-file line-to-report)
    (map #(conj (dropped %) %))
    (filter #(some report-safe? %))
    count))
