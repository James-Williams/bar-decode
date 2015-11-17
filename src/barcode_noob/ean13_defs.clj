(ns barcode-noob.ean13-defs
  (:gen-class))

(def left-odd-bits [
  '(0 0 0 1 1 0 1) ;; 0
  '(0 0 1 1 0 0 1) ;; 1
  '(0 0 1 0 0 1 1) ;; 2
  '(0 1 1 1 1 0 1) ;; 3
  '(0 1 0 0 0 1 1) ;; 4
  '(0 1 1 0 0 0 1) ;; 5
  '(0 1 0 1 1 1 1) ;; 6
  '(0 1 1 1 0 1 1) ;; 7
  '(0 1 1 0 1 1 1) ;; 8
  '(0 0 0 1 0 1 1) ;; 9
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

