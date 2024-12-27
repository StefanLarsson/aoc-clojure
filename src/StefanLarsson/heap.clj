(namespace StefanLarsson.heap)

;; First lets make a situation where the kays are the values
;; Binary heap
;;        1
;;    2      3
;;  4    5  6 7
;; 8 9 10
;; But we use zero-based indexing
;;        0
;;    1      2
;;  3    4  5 6
;; 7 8  9
;; Observation: left-child(i) = 2*i +1 right-child(i) = 2*i + 2)
(defn  make-bin-heap [] [])

(defn left [i] (+ 1 (* 2 i)))
(defn right [i] (+ 2 (* 2 i)))
(defn heapify [h i]
  "heap h index i"
  ;; weh have various cases: if the right child exists, the left one also does
  ;; but the left child existing does not mean the right one does
  ;;
  (let [
    val (h i)
    l (left i)
    l-val (h l)]
    (if
      (not l-val)
      h
      (let [r (right i)
        r-val (h r)]
        (if (not r-val) 
            ;; We only have the left child
            (if ( <= val l-val) h
                (recur (assoc (assoc h i l-val) l val) l))
            ;; we also have the right childZZ
          )
        )



