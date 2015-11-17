(ns bar-decode.validate-test
  (:require [clojure.test :refer :all])
  (:use bar-decode.validate))

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

