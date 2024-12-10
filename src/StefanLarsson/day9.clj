(ns StefanLarsson.day9
  (:require [clojure.string :as string])
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
