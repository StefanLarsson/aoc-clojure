(ns StefanLarsson.day3
  (:gen-class))

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
