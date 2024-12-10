(ns StefanLarsson.utils
  (:require [clojure.string :as string]))

(defn file-to-lines
  "Read a file and split into lines"
  [filename]
  (->> filename
    slurp
    string/split-lines))
