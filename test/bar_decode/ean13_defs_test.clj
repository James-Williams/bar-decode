(ns bar-decode.ean13-defs-test
  (:require [clojure.test :refer :all])
  (:use bar-decode.ean13-defs))

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

