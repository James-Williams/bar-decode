(ns bar-decode.image
  (:gen-class)
  (:require [clojure.string :as str])
  )

(defn grey-threshold
  "takes d in (0,1) and convert all values to 0 or 1"
  [d xs]
  (let [
         minimum   (apply min xs)
         maximum   (apply max xs)
         pivot     (int (+ minimum (* d (- maximum minimum))))
       ]
  (map #(if (< pivot %) 0 1) xs)))

(defn pgm-to-pixels
  [ls]
  (let [
         header (take 3 ls)
         data   (drop 3 ls)
         values (map read-string data)
         type   (nth header 0)
         dims   (str/split (nth header 2) #"\s")
         width  (read-string (nth dims 0))
         height (read-string (nth dims 1))
         rows   (partition width values) ]
    (assert (= type "P2"))
    (assert (= (count rows) height))
    rows))

(defn process-pgm
  [f filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (f (line-seq rdr)))))

