(ns bar-decode.validate
  (:use bar-decode.ean13-defs))

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

