(ns StefanLarsson.day7
  (:require [clojure.string :as string])
  (:require [StefanLarsson.utils :as utils])
  (:gen-class))

;; Day 7
;; 21037: 9 7 18 13
;; result: operand ...
;; operators + and *
;; evaluated left-to-right! (no precedence)
;; 3 + 4 * 5 = 17
;; lets start with silly recursion!

(defn parse-separated-numbers [s]
  (as-> s tval
    ( string/trim tval)
    (string/split tval #"[^\d]+")
    (map  read-string tval)))

(defn parse-calibration-equation [s]
  (let [
    [test-value-string operands-string] (string/split s #": ")
    test-value (read-string test-value-string)
    operands (parse-separated-numbers operands-string)
      ]
;    (println "test-value"  test-value)
;    (println "operands" operands)    
    (vector test-value operands)
))
;(defn xxx [test-value acc operands]
;  (cond (empty? operands) (= test-value acc)
;        :else (or (xxx test-value (+ acc (first operands)) (rest operands))
;            (xxx test-value (* acc (first operands)) (rest operands)))))

(defn xxx [test-value acc operands]
  (cond (empty? operands) (= test-value acc)
        :else (or (xxx test-value (+ acc (first operands)) (rest operands))
            (xxx test-value (* acc (first operands)) (rest operands)))))
        
(defn calibration-equation-solvable? [[ test-value operands]]
  (xxx test-value (first operands) (rest  operands))
  )

(defn yyy [test-value acc operands used-operators]
  (println  acc operands used-operators)
  (cond (empty? operands) (if  (= test-value acc) used-operators)
        :else (or (yyy test-value (+ acc (first operands)) (rest operands) (conj used-operators "+"))
            (yyy test-value (* acc (first operands)) (rest operands) (conj used-operators "*")))))

        
(defn calibration-equation-solution [[ test-value operands]]
  (yyy test-value 0 operands [])
  
  
  )
; I get the sum 16931 included
; Which line is this and why should it not be there and why do I get it?

; 16931: 568 2 9 16 529 1 8 1 3 8
; [* + + * + * * + * +]
; 568 * 2 = 1136
; 1136 + 2 = 1138
; 1138 + 9 = 1145
; output of purported solution::q

; 16931 () [* + + * + * * + * +]
; [* + + * + * * + * +]


(defn day7_1 []
  (let [
    equations (utils/parse-file parse-calibration-equation "resources/day7.txt")
      ]
  (->> equations
    (filter calibration-equation-solvable?)
    (map #(% 0))
    (apply +))))

(defn || [x y] (read-string (str x y)))

(defn zzz [test-value acc operands]
  (cond (empty? operands) (= test-value acc)
        :else (or 
          (zzz test-value (+ acc (first operands)) (rest operands))
          (zzz test-value (* acc (first operands)) (rest operands))
          (zzz test-value (|| acc (first operands)) (rest operands))
)))

(defn calibration-equation-solvable-2? [[ test-value operands]]
  (zzz test-value (first operands) (rest  operands))
  )

(defn day7_2 []
  (let [
    equations (utils/parse-file parse-calibration-equation "resources/day7.txt" )
      ]

  (->> equations
    (filter calibration-equation-solvable-2?)
    (map #(% 0))
    (apply +))))

