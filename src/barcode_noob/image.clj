(ns barcode-noob.image
  (:gen-class)
  (:require [clojure.string :as str])
  )

(def threshold-delta 0.42)

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
         runs   (map first (compress row-bits))
       ]
    (assert (= type "P2"))
    (assert (= (count rows) height))
    (list runs)))

(defn read-pgm
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (process-pgm-lines threshold-delta (line-seq rdr)))))

