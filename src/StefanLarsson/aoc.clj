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
  (:gen-class))

    

;; Day 9 Disk Fragmentation

(defn to-ints [s]
  (->> s
    (re-find #"\d+")
    seq
    (map str)
    (map read-string)
  )
)

;; this is silly and only makes sense while there are only single-digit file ids
;; but lets make it anyway for fun
(defn to-dotty-string [ints]
  (loop [res "" next-id 0 ints ints is-file true]
    (cond
      (empty? ints) res
      :else (let [
        next-length (first ints)
        next-char (if is-file (first (str next-id))
          \.)]
          (recur
            (string/join (conj (repeat next-length (str next-char)) res))
            (if is-file (inc next-id) next-id)
            (rest ints)
            (not is-file)
          )
        )
    )
  )
)

; Assumption: a file block will not be length zero!
(defn build-queues [ints]
  (loop [ result { :used-blocks (sorted-map-by (comp - compare)) :free-blocks (sorted-set) } ints ints is-file true next-file-id 0 current-pos 0]
    (if (empty? ints) result
      (let [
        length (first ints)
        next-pos (+ current-pos length)
        blocks (range current-pos next-pos)
      ]
      (if is-file
        (do
        ;  (println result )
        (recur
          (assoc result :used-blocks (into (result :used-blocks) (map #(vector % next-file-id) blocks)))
          (rest ints)
          false
          (inc next-file-id)  
          next-pos
        ))
        (recur
          (assoc result :free-blocks (into (result :free-blocks) blocks))
          (rest ints)
          true
          next-file-id
          next-pos)
      )
    )
  )
  )
)

(defn defragment [queues]
  (let [
    used-blocks (queues :used-blocks)
    highest-used-kv (first used-blocks)
    highest-used-pos (highest-used-kv 0)
    free-blocks (queues :free-blocks)
    lowest-free (first free-blocks)]
    (if (and
          highest-used-kv
          lowest-free 
          (< highest-used-pos 
   lowest-free)) queues
        (recur {
          :used-blocks (dissoc (assoc used-blocks lowest-free (highest-used-kv 1)) highest-used-pos)
          :free-blocks (conj (disj free-blocks lowest-free) highest-used-pos )
        }))))



(defn day9_1 []
  (let [input (slurp "resources/day9.txt")
        state (build-queues (to-ints input))]
    (apply + (map (fn [[pos val]] (* val pos)) ( (defragment state) :used-blocks)))))

(defn something [ints]
  (loop [
      current-pos 0
      next-file-id 0
      ints ints
      free-chunks (sorted-map)
      files '()
      is-file true]
    (if (empty? ints) {:free-chunks free-chunks :files files}
      (do
      (let [
        length (first ints)
        next-pos (+ current-pos length)
        ]
      (if is-file
        (recur
          next-pos
          (inc next-file-id)
          (rest ints)
          free-chunks
          (conj files {:id next-file-id :start-pos current-pos :length length})
          false
        )
        (recur
          next-pos
          next-file-id
          (rest ints)
          (conj free-chunks [current-pos length])
          files
          true
        )
      )
      ))
    )
  )
)

(defn defrag-moving-files [thingy]
  (loop [ free-chunks (thingy :free-chunks)
          candidate-move-files (thingy :files)
          handled-files #{}]
      (do
      (if (empty? candidate-move-files)
          handled-files
          (let [
            file (first candidate-move-files)
            file-length (file :length)
            start-pos (file :start-pos)
            rest-files (rest candidate-move-files)
            free-chunk (first (filter #(and ( >= (% 1) file-length) (< (% 0) start-pos)) free-chunks))]
            (do
            (println file)
            (if free-chunk
              (recur
                (assoc (dissoc free-chunks (free-chunk 0)) (+ file-length (free-chunk 0)) (- (free-chunk 1) file-length))
                rest-files
                (conj handled-files (assoc file :start-pos (free-chunk 0))))
              (recur
                free-chunks
                rest-files
                (conj handled-files file)))))))))

(defn checksum2-file [file]
  (let [
    id (file :id)
    length (file :length)
    start-pos (file :start-pos)
  ]
  (* (file :id) (/ (* length (+ (* 2 start-pos) (dec length))) 2))))

(defn checksum2 [handled-files]
  (apply + (map checksum2-file  handled-files)))

(defn day9_2 []
    (-> "resources/day9.txt"
      slurp
      to-ints
      something
      defrag-moving-files
      checksum2
  ))
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
  9 [day9_1 day9_2 ]
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

