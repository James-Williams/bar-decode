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

(def left-parity-vec [
  '(:odd  :odd  :odd  :odd  :odd  :odd ) ;; 0
  '(:odd  :odd  :even :odd  :even :even) ;; 1
  '(:odd  :odd  :even :even :odd  :even) ;; 2
  '(:odd  :odd  :even :even :even :odd ) ;; 3
  '(:odd  :even :odd  :odd  :even :even) ;; 4
  '(:odd  :even :even :odd  :odd  :even) ;; 5
  '(:odd  :even :even :even :odd  :odd ) ;; 6
  '(:odd  :even :odd  :even :odd  :even) ;; 7
  '(:odd  :even :odd  :even :even :odd ) ;; 8
  '(:odd  :even :even :odd  :even :odd ) ;; 9
])

