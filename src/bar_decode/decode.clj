(ns bar-decode.decode
  (:use bar-decode.ean13-defs)
  (:use bar-decode.validate))

(def NUM-DIGIT-MATCHES 1)

(defn first-digit-from-parity
 [parity-list]
 (->> parity-list
   (.indexOf left-parity-vec)
   (#(if (= % -1) nil %))))

(defn compress [s]
  (->> (partition-by identity s) (map (juxt count first))))

(defn scale-list [xs]
  (let [ sum (reduce + xs) ]
    (map #(/ % sum) xs)))

(defn process-bits [xs]
  (let [ compressed (compress xs) ]
    (assert (= (count compressed) 4))
    (->>  compressed
          (map first)
          scale-list
      )))

(defn abs [v] (max v (- v)))

(defn dist
  [l1 l2]
  (assert (= (count l1) (count l2)))
  (let [ diffs (map #(abs (- %1 %2)) l1 l2)
       ]
    (reduce + diffs)))

(defn match-left-digit
  [scaled-run]
  (assert (= (count scaled-run) 4))
  (assert (= (reduce + scaled-run) 1))
  (let [ scaled-odd  (map process-bits left-odd-bits)
         scaled-even (map process-bits left-even-bits)
         scaled-digits (concat scaled-odd scaled-even)
         dist-vec (map #(dist scaled-run %) scaled-digits)
        ]
    (->> dist-vec
         (map vector (range))
         (sort-by #(nth % 1))
         (map first)
         (take NUM-DIGIT-MATCHES)
         (map #(vector (mod % 10) (if (< % 10) :odd :even)))
    )))

(defn extract-first-digit
  [left-digit-parity-pairs]
  (let [ first-digit (->> left-digit-parity-pairs
           (map #(drop 1 %))
           (map first)
           first-digit-from-parity
           )
         left-group (map first left-digit-parity-pairs)
         ]
    (if (= first-digit nil) 
      nil 
      (cons first-digit left-group))))

(defn match-right-digit
  [scaled-run]
  (assert (= (count scaled-run) 4))
  (assert (= (reduce + scaled-run) 1))
  (let [ scaled-digits  (map process-bits right-bits)
         dist-vec (map #(dist scaled-run %) scaled-digits)
        ]
    (->> dist-vec
         (map vector (range))
         (sort-by #(nth % 1))
         (map first)
         (take NUM-DIGIT-MATCHES)
    )))

(defn split-runs
  "Given a set of run counts (c,v), return a list of possible matches
   starting at begining of the sequence"
  [run-pairs]
  (let [ start-marker   (map #(first (drop 1 %)) (take 3 run-pairs))
         minimum-count  (+ 3 24 5 24 3) ]
    (if (not (and (= start-marker '(1 0 1)) (>= (count run-pairs) minimum-count)))
        (list) 
        (let [ runs           (map first run-pairs)
               left-groups    (partition 4 (take 24 (drop 3 runs)))
               center-marker  (take 5 (drop 27 run-pairs))
               right-groups   (partition 4 (take 24 (drop 32 runs)))
               left-matches   (map match-left-digit (map scale-list left-groups))
               right-matches  (map match-right-digit (map scale-list right-groups))
               left-guess-maybe (extract-first-digit (map first left-matches))
               right-guess    (map first right-matches)
               guess          (if (= left-guess-maybe nil) 
                                (list) 
                                (concat left-guess-maybe right-guess)) ]
           (filter validate-digits? (vector guess))))))

(defn scan-list
  [xs]
  (if (= xs (list)) (list) 
    (if (= (count xs) 1)
      (list xs)
      (cons xs (scan-list (rest xs))))))

(defn scan-row
  "Given a set of run counts (c,v), tries to find a barcode at each 
   horizontal position"
  [run-pairs]
  (reduce concat (map split-runs (scan-list run-pairs))))

(defn process-row [row] (scan-row (compress row)))

