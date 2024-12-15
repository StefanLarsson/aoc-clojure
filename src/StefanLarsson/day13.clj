(ns StefanLarsson.day13
  (:require [clojure.string :as string])
  (:gen-class))

(defn build [fn]
  (let [
    text (slurp fn)
    machine-strings (string/split text #"\n\n")  ]
  machine-strings
  ))


(defn parse-button [s]
  (let [m (re-matches #"Button .: X\+(\d+), Y\+(\d+)" s)]
    (mapv read-string (drop 1 m))))

(defn parse-prize [s]
  (let [m (re-matches #"Prize: X=(\d+), Y=(\d+)" s)]
  (mapv read-string (drop 1 m)))
)

(defn det [[u1 v1] [u2 v2]]
  (- (* u1 v2) (* u2 v1)))

(defn build-machine [s]
  (let [[a b prize] (string/split-lines s)
    a-button (parse-button a)
    b-button (parse-button b) ]
  {:a  a-button :b b-button :prize (parse-prize prize)
    :det (det a-button b-button)}))

(defn correct-prize [prize]
  (mapv #(+ 10000000000000 %) prize))

(defn correct-machine [m]
  (assoc m :prize (correct-prize (m :prize)))
)

(defn solve-machine [machine]
  {
    :x (/ (det (machine :prize) (machine :b)) (machine :det))
    :y (/ (det (machine :a) (machine :prize)) (machine :det))
 })

; Observation shows solutions where x and y are integers coincide!

(defn read-file [fn]
  (map build-machine (build fn)))

(defn read-file-correct [fn]
  (map correct-machine (map build-machine (build fn))))
