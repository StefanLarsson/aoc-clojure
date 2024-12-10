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
    (apply +  (map #(count (re-seq #"(?=(XMAS|SAMX))" %) ) all-strings))
))

(defn internal-positions [side]
  (for [i (range 1 (- side 1))
        j (range 1 (- side 1))]
      (vector i j)))

(defn is-center-MS2 [ x y ss]
  (and 
  (or 
    (and  (= \M (char-at-pos (- x 1) (- y 1) ss)) (= \S (char-at-pos (+ x 1) (+ y 1) ss)  ))
    (and  (= \S (char-at-pos (- x 1) (- y 1) ss)) (= \M (char-at-pos (+ x 1) (+ y 1) ss)  )))
  (or 
    (and  (= \M (char-at-pos (- x 1) (+ y 1) ss)) (= \S (char-at-pos (+ x 1) (- y 1) ss)  ))
    (and  (= \S (char-at-pos (- x 1) (+ y 1) ss)) (= \M (char-at-pos (+ x 1) (- y 1) ss)  )))))

(defn is-center-MS [x y ss]
  (and
    (or
      (and (= \M (char-at-pos (- x 1) (- y 1) ss) (= \S (char-at-pos (+ 1 x) (+ 1 y) ss))))
      (and (= \S (char-at-pos (- x 1) (- y 1) ss) (= \M (char-at-pos (+ 1 x) (+ 1 y) ss))))
    )
    ;(or
    ;  (and (= \M (char-at-pos (- x 1) (+ y 1) ss) (= \S (char-at-pos (+ 1 x) (- 1 y) ss))))
    ;  (and (= \S (char-at-pos (- x 1) (+ y 1) ss) (= \M (char-at-pos (+ 1 x) (- 1 y) ss))))
    ;i)
))
;; Find X-MAS:
;; The X-MASes correspond to the A:s in their centres.
;; We can find all the positions of A:s
;; And then verify they are center's om MAS in both diagonals
(defn day4_2 [] 
  (let [
    lines ( file-to-lines "resources/day4.txt")
    side (count lines)
    a-positions (filter #(= \A ( char-at-pos ( % 0) (% 1) lines)) ( internal-positions side))]
    (count  (filter #(is-center-MS2 (% 0) (% 1) lines) a-positions))
    ;a-positions
))

; guard position is marked by ^ (we assume he is pointing north always)
(defn guard-coordinate-if-found [index line]
  (let [p (string/index-of line "^")]
    (if p (vector p index)
      nil)))

(defn find-guard-position [lines]
  "Find the position of the guard. Returns vector of x and y coordinates"
  (first  (keep-indexed guard-coordinate-if-found lines )))

(defn is-free? [[x y] lines]
  ;(println "CCC"  x y lines)
  ;(println (.charAt (lines y )x))
  (not (= \# (.charAt ( lines y) x))))

(defn on-board? [[x y] lines]
  (and 
    (< -1 x (count (lines 0)))
    (< -1 y (count lines))))

(defn next-position [[ x y] dir]
  (cond (= dir :up ) (vector x (dec y))
        (= dir :down ) (vector x (inc y))
        (= dir :left ) (vector (dec x) y)
        (= dir :right) (vector (inc x) y)))

(defn next-dir [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))

; No detection of infinite loops!


(defn next-state [[x y] dir on-board? is-free?]
  (loop [ [x y] [x y] dir dir]

  (let [try-pos (next-position [x y] dir)]
    (cond
      (not (on-board?  try-pos )) (vector try-pos dir )
      (is-free? try-pos)  (vector  try-pos dir )
      :else (recur [x y] (next-dir dir) )
  ))))

(defn calculate-visited-positions [[x y] dir lines]
  (let [h (count lines)
    w (count (lines 0))
    on-board? (fn [[x y]] (and (< -1 x w) (< -1 y  h) ))
    is-free?  (fn [[x y]] (not (= \# (.charAt (lines y) x))))]
  (loop [[x y] [x y] dir dir visited #{}]
    (let [stepped (next-state [x y] dir on-board? is-free?)]
    (if (not (on-board? (stepped 0) )) (conj  visited [x y])
      (recur (stepped 0) (stepped 1) (conj visited [x y])))))))

(defn day6_1 []
  (let [
    lines (file-to-lines "resources/day6.txt")
    initial-position (find-guard-position lines)]
    (count  (calculate-visited-positions initial-position :up lines))
  ))

(defn is-cyclic? [[x y] dir lines [xo yo]]
  (let [
    h (count lines)
    w (count (lines 0))
    on-board? (fn [[x y]] (and (< -1 x w) (< -1 y h)))
    is-free? (fn [[x y]] (and (not (= \# (.charAt (lines y) x))) (not (= [x y] [xo yo]))))]
  (loop [[x y] [x y] dir dir seen-states #{}]
    (let [stepped (next-state [x y] dir on-board? is-free?)]
      (cond
        (not ( on-board? (stepped 0) )) false
        (contains? seen-states stepped) true
        :else (recur (stepped 0) (stepped 1) (conj seen-states [[x y] dir])))))))
 
(defn day6_2 []
  (let [
    lines (file-to-lines "resources/day6.txt")
    initial-position (find-guard-position lines)
    potential-obstacles (filter #(not (= initial-position %)) (for [ xo (range (count lines)) yo (range (count (lines 0)))] [xo yo]))
    ]
    (count (filter #(is-cyclic? initial-position :up lines %) potential-obstacles))
    ;initial-position
    ;(is-cyclic? initial-position :up lines [3 6])
  ))

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
    ;lines (file-to-lines "resources/example_day7.txt")
    lines (file-to-lines "resources/day7.txt")
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
    ;lines (file-to-lines "resources/example_day7.txt")
    lines (file-to-lines "resources/day7.txt")
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
            rest-files (rest candidate-move-files)
            free-chunk (first (filter #(>= (% 1) file-length) free-chunks))]
            (if free-chunk
              (recur
                (assoc (dissoc free-chunks (free-chunk 0)) (+ file-length (free-chunk 0)) (- (free-chunk 1) file-length))
                rest-files
                (conj handled-files (assoc file :start-pos (free-chunk 0))))
              (recur
                free-chunks
                rest-files
                (conj handled-files file))))))))

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
  (let [input (slurp "resources/example_day9.txt")
        ints (to-ints input)
        thingy (something ints)]
    
    (defrag-moving-files thingy)
    (-> "resources/day9.txt"
      slurp
      to-ints
      something
      defrag-moving-files
  ;    checksum2
  )))

(defn input-filename [day use-example]
  (string/join (concat ["resources/"] (if (use-example) ["example_"] []) ["day"] [(str day) ".txt"])))

(def days {
  2 [day2_1 day2_2]
  3 [day3_1 day3_2]
  4 [day4_1 day4_2]
  6 [day6_1 day6_2]
  7 [day7_1 day7_2]
 ; 8 [day8_1 ]}
  9 [day9_1 day9_2 ]
  }
)

(defn all
  "Do all the things we have solutions for"
  []
  (do
    (day1_2)
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

