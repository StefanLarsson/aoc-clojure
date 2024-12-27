(ns StefanLarsson.utils
  (:require [clojure.string :as string]))

(defn file-to-lines
  "Read a file and split into lines"
  [filename]
  (->> filename
    slurp
    string/split-lines))


; Let's really do some stuff that we could move to utils for file parsing
; Separate cases of all lines same and different parts?

; First : parse each line of a file into a seq of integers
; second : parse each line of a file into a single integer
(defn file-name-to-lines [fname]
  (let [ text (slurp fname)
    lines (string/split-lines text)]
  lines))

(defn file-name-to-ints [fname]
  (map read-string (file-name-to-lines fname)))

(defn string-to-ints [s re]
  (as-> s v
    (string/split v re)
    (map #(read-string %) v)))

; ints separated by whitespace
(defn file-name-to-int-seq-by-whitespace [fname]
  (->> fname
    slurp
    string/split-lines
    (map #(string-to-ints % #"\s+"))))

; Single group of lines as string, parsed by one function
(defn parse-lines [s f]
  (->> s
    (string/split-lines)
    (map f)))

; File with all lines parsed by same function
(defn parse-file [fname f]
  (->> fname
    slurp
    (string/split-lines)
    (map f)))
; Let's really do some stuff that we could move to utils for file parsing
; Separate cases of all lines same and different parts?

; First : parse each line of a file into a seq of integers
; second : parse each line of a file into a single integer
(defn file-name-to-lines [fname]
  (let [ text (slurp fname)
    lines (string/split-lines text)]
  lines))

(defn file-name-to-ints [fname]
  (map read-string (file-name-to-lines fname)))

(defn string-to-ints [s re]
  (as-> s v
    (string/split v re)
    (map #(read-string %) v)))

; ints separated by whitespace
(defn file-name-to-int-seq-by-whitespace [fname]
  (->> fname
    slurp
    string/split-lines
    (map #(string-to-ints % #"\s+"))))

; Single group of lines as string, parsed by one function
(defn parse-lines [s f]
  (->> s
    (string/split-lines)
    (map f)))

; File with all lines parsed by same function
(defn parse-file [f fname ]
  (->> fname
    slurp
    (string/split-lines)
    (map f)))


(defn split-on-empty-lines [s]
  (string/split s #"\n\n"))

(defn parse-integers-split-on-anything [s]
  (map read-string (string/split s #"[^\d]+")))

(defn  slurp-and-parse-by-sections[fname parsers]
  (->> fname
    slurp
    split-on-empty-lines
    (map string/split-lines)
    (map #(map %1 %2)  parsers)
    ))
