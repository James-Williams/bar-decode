(ns barcode-noob.core
  (:gen-class)
  (:use clojure.test)
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn check-digit
  "Produces the check digit from a list of digits"
  [ds]
  (let [products (map * (reverse ds) (cycle '(3 1)))]
    (- 10 (mod (reduce + products) 10))
  ))

(defn validate-digits?
  "Returns true if the last digit is correct"
  [full-ds]
  (let [ rev-d (reverse full-ds)
         last-d (first rev-d)
         ds (reverse (rest rev-d))]
    (= last-d (check-digit ds))
  ))

(def left-odd-bits [
  '(0 0 0 1 1 0 1)
  '(0 0 1 1 0 0 1)
  '(0 0 1 0 0 1 1)
  '(0 1 1 1 1 0 1)
  '(0 1 0 0 0 1 1)
  '(0 1 1 0 0 0 1)
  '(0 1 0 1 1 1 1)
  '(0 1 1 1 0 1 1)
  '(0 1 1 0 1 1 1)
  '(0 0 0 1 0 1 1)
  ])

(def right-bits
  (let [
         bitwise-complement (fn [x] (map #(- 1 %) x))
         ]
    (vec (map bitwise-complement left-odd-bits))))

(def left-even-bits
  (vec (map reverse right-bits)))
