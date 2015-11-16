(ns barcode-noob.core-test
  (:require [clojure.test :refer :all]
            [barcode-noob.core :refer :all]))

(deftest test-validate-digits-true
  (is (validate-digits? '(4 0 0 6 3 8 1 3 3 3 9 3 1))) ;; Wiki Main Picture
  (is (validate-digits? '(5 9 0 1 2 3 4 1 2 3 4 5 7))) ;; Wiki Example
  (is (validate-digits? '(8 7 1 1 2 5 3 0 0 1 2 0 2))) ;; Wiki Example
  (is (validate-digits? '(9 7 8 0 5 9 6 5 1 4 9 8 3))) ;; Real World Haskell
)

(deftest test-validate-digits-false
  (is (not (validate-digits? '(4 0 0 6 3 8 1 3 3 3 9 3 2))))
  (is (not (validate-digits? '(5 9 0 1 2 3 4 4 2 3 4 5 7))))
)

(deftest test-bit-tables
  (is (= (get left-odd-bits 3)   '(0 1 1 1 1 0 1)))
  (is (= (get left-odd-bits 9)   '(0 0 0 1 0 1 1)))
  (is (= (get left-even-bits 4)  '(0 0 1 1 1 0 1)))
  (is (= (get left-even-bits 6)  '(0 0 0 0 1 0 1)))
  (is (= (get right-bits 0)      '(1 1 1 0 0 1 0)))
  (is (= (get right-bits 7)      '(1 0 0 0 1 0 0)))
)

(deftest test-left-parity-vec
  (is (= (get left-parity-vec 5) '(:odd  :even :even :odd  :odd  :even)))
  (is (= (get left-parity-vec 9) '(:odd  :even :even :odd  :even :odd )))
)

(deftest test-enc-barcode
  (is (= (enc-barcode '(8 7 1 1 2 5 3 0 0 1 2 0)) (apply concat (list
           '(1 0 1)         ;; Start Marker
           '(0 1 1 1 0 1 1) ;; 7 - Odd
           '(0 1 1 0 0 1 1) ;; 1 - Even
           '(0 0 1 1 0 0 1) ;; 1 - Odd
           '(0 0 1 1 0 1 1) ;; 2 - Even
           '(0 1 1 1 0 0 1) ;; 5 - Even
           '(0 1 1 1 1 0 1) ;; 3 - Odd
           '(0 1 0 1 0)     ;; Center Marker
           '(1 1 1 0 0 1 0) ;; 0
           '(1 1 1 0 0 1 0) ;; 0
           '(1 1 0 0 1 1 0) ;; 1
           '(1 1 0 1 1 0 0) ;; 2
           '(1 1 1 0 0 1 0) ;; 0
           '(1 1 0 1 1 0 0) ;; 2 (Check-Digit)
           '(1 0 1)         ;; End Marker
           ))))
  (is (= (enc-barcode '(9 7 8 0 5 9 6 5 1 4 9 8)) (apply concat (list
           '(1 0 1)         ;; Start Marker
           '(0 1 1 1 0 1 1) ;; 7 - Odd
           '(0 0 0 1 0 0 1) ;; 8 - Even
           '(0 1 0 0 1 1 1) ;; 0 - Even
           '(0 1 1 0 0 0 1) ;; 5 - Odd
           '(0 0 1 0 1 1 1) ;; 9 - Even
           '(0 1 0 1 1 1 1) ;; 6 - Odd
           '(0 1 0 1 0)     ;; Center Marker
           '(1 0 0 1 1 1 0) ;; 5
           '(1 1 0 0 1 1 0) ;; 1
           '(1 0 1 1 1 0 0) ;; 4
           '(1 1 1 0 1 0 0) ;; 9
           '(1 0 0 1 0 0 0) ;; 8
           '(1 0 0 0 0 1 0) ;; 3 (Check-Digit)
           '(1 0 1)         ;; End Marker
           ))))
)

