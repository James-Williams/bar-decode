(ns barcode-noob.decode-test
  (:require [clojure.test :refer :all])
  (:use barcode-noob.decode))

(deftest test-first-digit-from-parity
  (is (= (first-digit-from-parity '(:odd  :odd  :even :even :even :odd )) 3))
  (is (= (first-digit-from-parity '(:odd  :even :odd  :odd  :even :even)) 4))
  (is (= (first-digit-from-parity '(:even :even :odd  :odd  :even :even)) nil)))

(def example-image-expected '(9 7 8 0 1 3 2 1 1 4 6 7 7))
(def example-image-line '(22 2 2 2 2 7 2 5 7 1 6 2 2 2 4 6 6 4 4 2 2 3 9 2 4 2 5 4 2 3 1 3 2 4 5 5 1 5 4 5 1 3 2 7 5 1 2 2 10 2 7 1 6 1 7 2 5 2 1 3 7))
(def example-image-line-pairs
  (map vector example-image-line (cycle '(0 1))))

;; Need to manually find the start of the barcode using (rest ..)
(deftest test-split-runs
  (is (= (first (split-runs (rest example-image-line-pairs))) 
         example-image-expected)))

;; Scans across the line for a valid barcode
(deftest test-split-runs
  (is (= (first (scan-row example-image-line-pairs))
         example-image-expected)))

(deftest test-scan-list
  (is (= (scan-list (list)) (list)))
  (is (= (scan-list (list :d)) (list (list :d))))
  (is (= (scan-list '(:a :b :c)) (list '(:a :b :c) '(:b :c) (list :c))))
)

