(ns mikera.vectorz.test-matrix-api
  (:use [clojure test])
  (:use [core matrix])
  (:use core.matrix.operators)
  (:require core.matrix.compliance-tester)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:require core.matrix.impl.persistent-vector)
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector])
  (:refer-clojure :exclude [vector? * - +]))

;; note - all the operators are core.matrix operators

(set-current-implementation :vectorz)

(deftest test-vector-ops
  (testing "addition"
    (is (= (v/of 1 2) (+ (v/of 1 1) [0 1])))
    (is (= (v/of 3 4) (+ (v/of 1 1) (v/of 2 3))))
    (is (= [1.0 2.0] (+ [0 2] (v/of 1 0)))))
  
  (testing "scaling"
    (is (= (v/of 2 4) (* (v/of 1 2) 2)))
    (is (= (v/of 2 4) (scale (v/of 1 2) 2)))
    (is (= (v/of 2 4) (scale (v/of 1 2) 2.0))))
  
  (testing "subtraction"
    (is (= (v/of 2 4) (- (v/of 3 5) [1 1])))
    (is (= (v/of 1 2) (- (v/of 2 3) (v/of 1 0) (v/of 0 1))))))

(deftest test-matrix-ops
  (testing "addition"
    (is (= (m/matrix [[2 2] [2 2]]) (+ (m/matrix [[1 1] [2 0]]) 
                                       (m/matrix [[1 1] [0 2]]))))
    (is (= (m/matrix [[2 2] [2 2]]) (+ (m/matrix [[1 1] [2 0]]) 
                                       [[1 1] [0 2]])))
    (is (= [[2.0 2.0] [2.0 2.0]] (+ [[1 1] [0 2]]
                                    (m/matrix [[1 1] [2 0]])))))
  (testing "scaling"
    (is (= (m/matrix [[2 2] [2 2]]) (scale (m/matrix [[1 1] [1 1]]) 2))))
  
  (testing "multiplication"
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) (m/matrix [[2] [2]]))))
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) [[2] [2]])))
    ;; (is (= [[8.0]] (* [[2 2]] (m/matrix [[2] [2]]))))
    ))

(deftest test-matrix-transform
  (testing "vector multiple"
    (is (= (v/of 2 4) (* (m/matrix [[2 0] [0 2]]) (v/of 1 2))))
    (is (= (v/of 2 4) (* (m/scalar-matrix 2 2.0) (v/of 1 2))))
    (is (= (v/of 2 4) (* (m/scalar-matrix 2 2.0) [1 2]))))
  (testing "persistent vector transform"
    (is (= (v/of 1 2) (transform (m/identity-matrix 2) [1 2]))))
  (testing "transform in place"
    (let [v (matrix [1 2])
          m (matrix [[2 0] [0 2]])] 
      (transform! m v)
      (is (= (v/of 2 4) v)))))

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

(deftest test-construction
  (testing "1D"
    (is (= (v/of 1.0) (matrix [1]))))
  (testing "2D"
    (is (= (m/matrix [[1 2] [3 4]]) (matrix [[1 2] [3 4]])))))

(deftest test-vector-conversion
  (testing "vector" 
    (is (= [1.0] (to-nested-vectors (v/of 1.0)))))
  (testing "vector" 
    (is (= [[1.0]] (to-nested-vectors (m/matrix [[1.0]]))))))

;; run compliance test
(deftest compliance-test
  (core.matrix.compliance-tester/compliance-test (v/of 1 2))) 
