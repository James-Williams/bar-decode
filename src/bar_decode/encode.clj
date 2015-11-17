(ns bar-decode.encode
  (:use bar-decode.ean13-defs)
  (:use bar-decode.validate))

(defn enc-left-group
  "given a seq of digits, generate a seq of bits for the left group"
  [first-digit ds]
  (assert (= (count ds) 6))
  (let [
         parity-list (get left-parity-vec first-digit)
         left-vecs (map #(case % :odd  left-odd-bits
                                 :even left-even-bits) parity-list)]
    (map #(get %1 %2) left-vecs ds)))

(defn enc-right-group
  "given a seq of digits, generate a seq of bits for the right group"
  [ds]
  (assert (= (count ds) 6))
  (map #(get right-bits %) ds))

(defn enc-digits
  "given a seq of 12 digits, generate a seq of bits for the entire barcode"
  [d-in]
  (assert (= (count d-in) 12))
  (let [
         last-digit (check-digit d-in)
         ds           (concat d-in (list last-digit))
         start-bits   (list '(1 0 1))
         end-bits     start-bits
         center-bits  (list '(0 1 0 1 0))
         first-digit  (first ds)
         left-digits  (take 6 (drop 1 ds))
         right-digits (drop 7 ds)
         left-bits    (enc-left-group first-digit left-digits)
         right-bits   (enc-right-group right-digits)]
    (apply concat (concat start-bits left-bits center-bits right-bits end-bits))))
