(ns mikera.vectorz.test-linear
  (:use [clojure.test]
        [clojure.core.matrix]
        [clojure.core.matrix.linear])
  (:require [mikera.vectorz.core :as v]))

(set-current-implementation :vectorz)

(deftest test-svd
  (let [result (svd [[2 0] [0 1]])]
    (is (every? v/vectorz? (vals result)))
    (is (equals [2 1] (:S result)))
    (is (every? orthogonal? ((juxt :V* :U) result)))))

;(deftest test-solve
;  (let [A [[1 2]
;           [2 1]]
;        b [22
;           26]]
;    (is (equals [10 6] (solve A b)))))