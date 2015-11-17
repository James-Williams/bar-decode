(ns barcode-noob.image
  (:gen-class)
  (:require [clojure.string :as str])
  (:use barcode-noob.core)
  )

(def THRESHOLD-DELTA 0.42)
(def NUM-DIGIT-MATCHES 1)

(defn grey-threshold
  "takes d in (0,1) and convert all values to 0 or 1"
  [d xs]
  (let [
         minimum   (apply min xs)
         maximum   (apply max xs)
         pivot     (int (+ minimum (* d (- maximum minimum))))
       ]
  (map #(if (< pivot %) 0 1) xs)))

(defn compress [s]
  (->> (partition-by identity s) (map (juxt count first))))

(defn scale-list [xs]
  (let [ sum (reduce + xs) ]
    (map #(/ % sum) xs)))

(defn process-bits [xs]
  (let [ compressed (compress xs) ]
    (assert (= (count compressed) 4))
    (->>  compressed
          (map first)
          scale-list
      )))

(defn abs [v] (max v (- v)))

(defn dist
  [l1 l2]
  (assert (= (count l1) (count l2)))
  (let [ diffs (map #(abs (- %1 %2)) l1 l2)
       ]
    (reduce + diffs)))

(defn match-left-digit
  [scaled-run]
  (assert (= (count scaled-run) 4))
  (assert (= (reduce + scaled-run) 1))
  (let [ scaled-odd  (map process-bits left-odd-bits)
         scaled-even (map process-bits left-even-bits)
         scaled-digits (concat scaled-odd scaled-even)
         dist-vec (map #(dist scaled-run %) scaled-digits)
        ]
    (->> dist-vec
         (map vector (range))
         (sort-by #(nth % 1))
         (map first)
         (take NUM-DIGIT-MATCHES)
         (map #(vector (mod % 10) (if (< % 10) :odd :even)))
    )))

(defn extract-first-digit
  [left-digit-parity-pairs]
  (let [ first-digit (->> left-digit-parity-pairs
           (map #(drop 1 %))
           (map first)
           first-digit-from-parity
           )
         left-group (map first left-digit-parity-pairs)
         ]
    (cons first-digit left-group)))

(defn match-right-digit
  [scaled-run]
  (assert (= (count scaled-run) 4))
  (assert (= (reduce + scaled-run) 1))
  (let [ scaled-digits  (map process-bits right-bits)
         dist-vec (map #(dist scaled-run %) scaled-digits)
        ]
    (->> dist-vec
         (map vector (range))
         (sort-by #(nth % 1))
         (map first)
         (take NUM-DIGIT-MATCHES)
    )))

(defn process-row
  [row]
  (let [
         runs   (map first (compress row))
         digit-runs (drop 4 runs)
       ]
    digit-runs))


(defn process-pgm-lines
  [delta ls]
  (let [
         header (take 3 ls)
         data   (drop 3 ls)
         values (map read-string data)
         type   (nth header 0)
         dims   (str/split (nth header 2) #"\s")
         width  (read-string (nth dims 0))
         height (read-string (nth dims 1))
         rows   (partition width values)
         row    (nth rows 21)
         row-bits (grey-threshold delta row)
       ]
    (assert (= type "P2"))
    (assert (= (count rows) height))
    (list (process-row row-bits))))

(defn read-pgm
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (process-pgm-lines THRESHOLD-DELTA (line-seq rdr)))))

;; TODO Test code from REPL - WORKING DECODE OF LEFT DIGITS!!!
;;        Need to build this into more functions!

(def l (first (read-pgm "barcode-example1.pgm")))
(def working-left (map match-left-digit (map scale-list (take 6 (partition 4 l)))))

(def r (drop (+ 5 (* 6 4)) l))
(def working-right (->> r
  (partition 4) 
  (take 6) 
  (map scale-list)
  (map match-right-digit)))

