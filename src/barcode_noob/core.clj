(ns barcode-noob.core
  (:gen-class)
  (:use barcode-noob.decode)
  (:use barcode-noob.image))

(def THRESHOLD-DELTA 0.42)

(defn process-pgm-lines
  [ls]
  (let [ rows (pgm-to-pixels ls)
         rows-bits (map #(grey-threshold THRESHOLD-DELTA %) rows)
       ]
    (reduce concat (map process-row rows-bits))))

(defn scan-pgm [filename] 
  (process-pgm process-pgm-lines filename))

(defn -main [& args]
  (if (not= (count args) 1)
    (println "Usage: lein run <pgm-filename>")
    (->> (first args)
         scan-pgm
         first
         println)))
