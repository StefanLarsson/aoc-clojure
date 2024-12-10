(ns StefanLarsson.day1
  (
    :require [StefanLarsson.utils :as utils]
  )
  (
    :require [clojure.string :as string]
  )
)

; Just copied from REPL
(defn day1_2 []

(def lines (utils/file-to-lines "resources/day1.txt"))
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
