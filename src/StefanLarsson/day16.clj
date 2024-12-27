(ns StefanLarsson.day16
  (:require [ StefanLarsson.utils :as utils])
  (:require [ clojure.string :as string])
)

(defn build-maze [lines]
  { :lines lines :w (count (first lines)) :h (count lines)})

(defn parse-file [fname]
  (-> fname
    slurp
    string/split-lines
    maze))

(defn find-pos-in-line [s c]
  (string/index-of s c))

(defn find-pos [maze c]
  (let [found-lines (map #(find-pos-in-line % c)  maze)]
  (keep-indexed #(when %2 [%2 %1]) found-lines))
  )

(defn find-initial-state [maze]
  { :pos (find-pos (:lines  maze) \S) :dir :east})

(def direction-deltas { :north [ 0 -1] :west [ -1 0] :east [1 0] :south [0 1]})

(defn turn-left-dir [dir]
  (condp = dir
    :north :west
    :west :south
    :south :east
    :east :north
    nil))
  
(defn turn-right-dir [dir]
  (condp = dir
    :north :east
    :west :north
    :south :west
    :east :south
    nil))

(defn move-forward-dir [pos dir]
  (map + pos (direction-deltas dir))
  )
; The example map is edged by walls
; Let's assume the real one is also for now
; Means stepping forward only needs to check for whether there is a wall
; not for whether we are outside the board
; maybe the w and h are needless then : (
; order of least cost first:
; forward; turn left; turn right
; If we keep the direction as part of the state this is all we need
(defn move-forward [state]
  (let [current-pos (:pos state)]
  (assoc state :pos (move-forward-dir current-pos (:dir state)))))

(defn turn-left [state]
  (let [current-dir (:dir state)]
    (assoc state :dir (turn-left-dir current-dir))))

(defn turn-right [state]
  (let [current-dir (:dir state)]
    (assoc state :dir (turn-right-dir current-dir))))

(defn potential-moves [state]
  (map (fn [ func cost] {:state (func state) :cost cost}) [ move-forward turn-left  turn-right ] [ 1 1000 1000]))

(defn is-free [[x y] maze]
  (not (= \# (.charAt ( (:lines maze) y) x))))

(defn allowed-moves [state maze]
  (let [ potential-moves  (potential-moves state)]
    (filter #(is-free ( (:state % ) :pos) maze) potential-moves)))

(defn dijkstraEverything [maze initial-state]
  (loop [state initial-state costs {initial-state 0} to-visit (into clojure.lang.PersistentQueue/EMPTY (allowed-moves initial-state maze)) ]
    (if (empty? to-visit) costs
      (let [
        am (allowed-moves state maze)
        
        updated-costs (reduce #(if (< (:cost %2)) (assoc %1 (:pos %2) (:cost %2)) %1) costs am)] (recur ))
    )
  
    ))
