(ns mikera.vectorz.test-matrix-api
  (:use [clojure test])
  (:use [core matrix])
  (:use core.matrix.operators)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector]))

;; note - all the operators are core.matrix operators

(deftest test-vector-ops
  (testing "addition"
    (is (= (v/of 1 2) (+ (v/of 1 1) [0 1])))
    (is (= [1.0 2.0] (+ [0 2] (v/of 1 0))))))

(deftest test-matrix-ops
  (testing "addition"
    (is (= (m/matrix [[2 2] [2 2]]) (+ (m/matrix [[1 1] [2 0]]) 
                                       (m/matrix [[1 1] [0 2]])))))
  (testing "scaling"
    (is (= (m/matrix [[2 2] [2 2]]) (scale (m/matrix [[1 1] [1 1]]) 2))))
  
  (testing "multiplication"
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) (m/matrix [[2] [2]]))))
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) [[2] [2]])))))

(deftest test-matrix-transform
  (testing "vector transform"
    (is (= (v/of 2 4) (* (m/matrix [[2 0] [0 2]]) (v/of 1 2))))))

;; verify scalar operators should still work on numbers!
(deftest test-scalar-operators
  (testing "addition"
    (is (== 2.0 (+ 1.0 1.0)))
    (is (== 3 (+ 1 2))))
  (testing "multiplication"
    (is (== 2.0 (* 4 0.5)))
    (is (== 6 (* 1 2 3))))
  (testing "subtraction"
    (is (== 2.0 (- 4 2.0)))
    (is (== 6 (- 10 2 2))))) 
