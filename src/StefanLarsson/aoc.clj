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
    ;lines (utils/file-to-lines "resources/example_day7.txt")
    lines (utils/file-to-lines "resources/day7.txt")
    equations (map parse-calibration-equation lines)
    x (filter calibration-equation-solvable? equations) 
      ]
  ;(println ( calibration-equation-solution [16931 [ 568 2 9 16 529 1 8 1 3 8]]))
  ;(println (count lines) " lines.")
  ;(println "Max # of operands: "  (apply max  (map #(count (% 1) ) equations)))
  ;(println "Min # of operands: "  (apply min  (map #(count (% 1) ) equations)))
  ;(println    (filter calibration-equation-solvable? equations))
  ;(println (map #(% 0)   (filter calibration-equation-solvable? equations)))
  ;(for [[result operands] (filter calibration-equation-solvable? equations)]
  ;  (do  (println result)))

  ;(println (reduce #(str %1 "\n" %2)   (map #(str (% 0)) x)))

  (apply +  (map #(% 0) (filter calibration-equation-solvable? equations)))
  
))

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
    ;lines (utils/file-to-lines "resources/example_day7.txt")
    lines (utils/file-to-lines "resources/day7.txt")
    equations (map parse-calibration-equation lines)
    x (filter calibration-equation-solvable-2? equations) 
      ]
  ;(println ( calibration-equation-solution [16931 [ 568 2 9 16 529 1 8 1 3 8]]))
  ;(println (count lines) " lines.")
  ;(println "Max # of operands: "  (apply max  (map #(count (% 1) ) equations)))
  ;(println "Min # of operands: "  (apply min  (map #(count (% 1) ) equations)))
  ;(println    (filter calibration-equation-solvable? equations))
  ;(println (map #(% 0)   (filter calibration-equation-solvable? equations)))
  ;(for [[result operands] (filter calibration-equation-solvable? equations)]
  ;  (do  (println result)))

  ;(println (reduce #(str %1 "\n" %2)   (map #(str (% 0)) x)))

  (apply +  (map #(% 0) (filter calibration-equation-solvable-2? equations)))
  
))

         
;; Day 8: Resonant Collinearity
  
;; Key info;
;; Puzzle input is map of antennas
;; Antinodes: points twice as far from one antenna as from the other
;; ....#.a.a.#....
;; ..#..b..b..#...
;; ...#cc#...
;; Antennae have frequencies
;; lowercase letter, uppercase letter, digit
;; Confused description! What about the harmonically conjugate points?
;; input consists of characters, then a newline, then more characters,...
(defn string-to-map2d [s]
  (let [width (string/index-of s \newline) l (count s)]
  { :mapstring s :width width  :height (/ l (inc width))})
)

(defn on-map? [[x y] map2d]
  (and
    (< -1 x (map2d :width)) (-1 y (map2d :height))
  )
)


(defn value-at [map2d [x y]]
  (.charAt (map2d :mapstring) (+ x  (* y (inc (map2d :width) )))))
; I want to find all pairs of antennae of same frequency 

(defn all-positions [map2d]
  (for [x (range (map2d :width))  y (range (map2d :height))]
    [x y]))

(defn map2d-group-by-value [map2d]
  (dissoc (group-by #(value-at map2d %)  (all-positions map2d)) \.))

(defn map2d-frequencies [map2d]
  (frequencies (map #(value-at map2d %) (all-positions map2d))))

; .....A...A........
; .#...A...A...#....
;      x1  x2
;  (x1 - (x2 - x1) = 2x1 - x2
;              x2 + (x2 - x1) = 2x2 - x1 
; this is the case where x2 > x1 !
; otherwise we will be at x1 + (x1 - x2) and x2 - (x1 -x2) which will be 
; the same two point just reordered!

(defn anti-nodes [[x1 y1] [x2 y2]]
  (let [dx1 (* 2 x1) dx2 (* 2 x2) dy1 (* 2 y1) dy2 (* 2 y2)]
    (vector [ (- dx1 x2) (- dy1 y2) ] [ (- dx2 x1) (- dy2 y1)])))

(defn day8_1 [] 
  (let [
    input (slurp "resources/day8.txt")
    map2d (string-to-map2d input)
    value-pos-map (map2d-group-by-value map2d)
  ]
  value-pos-map))
    

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
  7 [day7_1 day7_2]
 ; 8 [day8_1 ]}
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

