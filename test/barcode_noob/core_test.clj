(ns barcode-noob.core-test
  (:require [clojure.test :refer :all]
            [barcode-noob.core :refer :all]))

(deftest test-validate-digits-true
  (is (validate-digits? '(4 0 0 6 3 8 1 3 3 3 9 3 1)))
  (is (validate-digits? '(5 9 0 1 2 3 4 1 2 3 4 5 7)))
)

(deftest test-validate-digits-false
  (is (not (validate-digits? '(4 0 0 6 3 8 1 3 3 3 9 3 2))))
  (is (not (validate-digits? '(5 9 0 1 2 3 4 4 2 3 4 5 7))))
)
