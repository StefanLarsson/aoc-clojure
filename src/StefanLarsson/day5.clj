(ns StefanLarsson.day5
  (:require [StefanLarsson.utils :as utils])
  (:require [clojure.string :as string])
  (:gen-class))


(defn parse-ordering-rule [s]
  (let [[before after & scrap] (utils/parse-integers-split-on-anything s)]
   [before after])) 

(defn allowed-page-pair? [before after rule]
  (let [[rule-before rule-after] rule]
    (not (and (= rule-before after) (= rule-after before)))))

(defn page-allowed? [new-page seen-pages rule]
  (every? #(allowed-page-pair? % new-page rule) seen-pages))

(defn page-allowed-rules? [new-page seen-pages rules]
  (every? #(page-allowed? new-page seen-pages %) rules))

(defn update-allowed? [update rules]
  (loop [seen-pages #{} remaining-pages update]
    (cond
      (empty? remaining-pages) true
      (not (page-allowed-rules? (first remaining-pages) seen-pages rules)) false
      :else (recur (conj seen-pages (first remaining-pages)) (rest remaining-pages) )) ))

(defn middle [s]
  (nth s (/ (count s) 2)))

(defn sum-middle-page-correctly-ordered [fname]
  (as-> fname v
    (utils/slurp-and-parse-by-sections v [parse-ordering-rule utils/parse-integers-split-on-anything])
    (filter #(update-allowed? % (first v)) (second v))
    (map middle v)
    (apply + v)))

(defn day5_1 []
  (sum-middle-page-correctly-ordered "resources/day5.txt"))

(defn is-branch? [initial remaining rules]
  (println "b: " initial remaining)
  (let [
    allowed-nexts (filter #(page-allowed-rules? % initial rules) remaining)
    ]
    (not (empty? allowed-nexts))))

(defn children [initial remaining rules]
;  (println "c: " initial remaining)
  (let [
    allowed-nexts (filter #(page-allowed-rules? % initial rules) remaining)]
    (map #(vector (conj initial %) (disj remaining %)) allowed-nexts)))
  
(defn xxx [update rules]
  (tree-seq
    #(is-branch? (first %) (second %) rules)
    #(children (first %) (second %) rules)
    ['() (into #{} update)]))

(defn can-be-first-by-rule? [page pages rule]
  (every? #(allowed-page-pair? page % rule) pages))

(defn can-be-first? [page pages rules]
  (every? #(can-be-first-by-rule? page pages %) rules))

(defn candidate-first [pages rules]
  (first (filter #(can-be-first? % pages rules) pages)))

(defn find-correct-order [pages rules]
  (loop [result [] remaining (into #{} pages)]
    (if
      (empty? remaining)
      result
      (let [ next (candidate-first remaining rules)]
        (recur (conj result next) (disj remaining next))
        )
      )))

(defn sum-middle-page-reordered-incorrect [fname]
  (let [[rules updates & scrap] (utils/slurp-and-parse-by-sections fname [parse-ordering-rule utils/parse-integers-split-on-anything])]
    (->> updates
      (filter #((complement update-allowed?) % rules))
      (map #(find-correct-order % rules))
      (map middle)
      (apply +)
    )))

(defn day5_2 []
  (sum-middle-page-reordered-incorrect "resources/day5.txt")
)
