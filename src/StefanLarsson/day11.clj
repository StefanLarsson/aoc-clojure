(ns StefanLarsson.day11
  (:gen-class))


(defn transform-stone-text [string-value]
  "Assume the length of strng-value is event!"
  (let [ m (/ (count string-value) 2)]
    (->> string-value
      (split-at m)
      (map #(apply str %))
      (map  parse-long)
    )
  )
)
(defn transform-stone [value]
  (cond
    (= 0 value) '( 1)
    :else (let [string-value (str value)
      ]
    (if (= 0  (mod (count string-value) 2))  (transform-stone-text string-value)
        (list (* 2024 value))
    )
   )
  )
)

(defn step-state [stones]
    (flatten (map transform-stone stones)
))
(def memo-step-state (memoize step-state))

(defn iterate-transformation [initial-state n]
  (loop [state initial-state
    i 0]

    (do
      (println i)

    (if (= n i) state
        (recur (step-state state) (inc i)))))
)


(def input '( 4189 413 82070 61 655813 7478611 0 8))
(defn day11_1 []
  (let [
    input-string (slurp "resources/day11.txt")
    start-seq (read-string (str \( input-string \)))
    end-seq (iterate-transformation start-seq 25)
    ]
  (count end-seq) 
))


(defn num-stones-after-steps [value steps]
  (if (> steps 30) ( println (str  "calculating a value for " steps" steps")))
  (if (= 0 (mod steps 10 ) ) ( println (str  "calculating a value for " steps" steps")))
  (if
    (= 0 steps) 1
    (apply + (map #( num-stones-after-steps  % (dec steps)) (transform-stone value)))
     
))

(def memo-nsas (memoize num-stones-after-steps))

(defn day11_2 []
  (apply + (map #(memo-nsas % 75) input)
  ))

