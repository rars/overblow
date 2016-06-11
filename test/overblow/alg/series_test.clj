(ns overblow.alg.series-test
  (:require [overblow.alg.series :refer :all]
            [clojure.test :refer :all]))

(deftest should-increment-by-2-for-an-arith-prog
  (are [x y] (= x y)
    1 (first (arith-prog 1 2))
    3 (nth (arith-prog 1 2) 1)
    5 (nth (arith-prog 1 2) 2)))

(deftest should-increment-by-factor-2-for-a-geom-prog
  (are [x y] (= x y)
    1 (first (geom-prog 1 2))
    2 (nth (geom-prog 1 2) 1)
    4 (nth (geom-prog 1 2) 2)
    8 (nth (geom-prog 1 2) 3)))

(deftest should-return-harmonic-mean
  (are [x y] (= x y)
    (/ 12 7) (harmonic-mean 1 2 4)))
