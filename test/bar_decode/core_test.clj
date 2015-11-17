(ns bar-decode.core-test
  (:require [clojure.test :refer :all])
  (:use bar-decode.core))

;; Top level image decode test
(deftest test-scan-pgm
  (is (= (first (scan-pgm "barcode-example1.pgm"))
         '(9 7 8 0 1 3 2 1 1 4 6 7 7)))
  (is (= (first (scan-pgm "barcode-example3-crop.pgm"))
         '(9 7 8 0 5 9 6 5 1 4 9 8 3))))

