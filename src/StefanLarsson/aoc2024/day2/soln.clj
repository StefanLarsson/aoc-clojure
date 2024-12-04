(require '[clojure.string :as str])

(def lines (str/split-lines (slurp "input.txt")))

(def lines-arrays (map #(str/split % #" +") lines))
(def lines-int-arrays (map #(map read-string %) lines-arrays ))

