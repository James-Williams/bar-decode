(ns barcode-noob.encode-test
  (:require [clojure.test :refer :all])
  (:use barcode-noob.encode))

(deftest test-enc-digits
  (is (= (enc-digits '(8 7 1 1 2 5 3 0 0 1 2 0)) (apply concat (list
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
  (is (= (enc-digits  '(9 7 8 0 5 9 6 5 1 4 9 8)) (apply concat (list
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

