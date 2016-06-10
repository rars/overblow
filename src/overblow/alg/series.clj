(ns
  ^{:doc "Mathematical series."
    :author "Richard Russell"}
  overblow.alg.series)

(defn arith-prog
  " Creates a lazy-seq of the arithmetic progression starting
    at <start> and increasing at each step by <separation>."
  [start separation]
  (for [x (iterate (fn [y] (+ y separation)) start)] x))

(defn geom-prog
  " Creates a lazy-seq of the geometric progression starting at
    <start> and changing by the factor <ratio> at each step."
  [start ratio]
  (for [x (iterate (fn [y] (* y ratio)) start)] x))

(defn harmonic-mean
  " Returns the harmonic mean of the set of real numbers <args>."
  [& args]
  (/ (count args)
     (reduce + (map (fn [x] (/ 1 x)) args))))
