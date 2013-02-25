(ns mikera.vectorz.test-matrix
  (:use [clojure test])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(deftest test-constructors
  (testing "identity"
    (is (= (m/matrix [[1 0] [0 1]]) (m/identity-matrix 2)))
    (is (= (m/identity-matrix 3) (m/scale-matrix [1 1 1]))))
  (testing "scale matrix"
    (is (= (m/matrix [[1 0] [0 1]]) (m/scale-matrix 2 1))))
  (testing "diagonal matrix"
    (is (= (m/matrix [[2 0] [0 3]]) (m/diagonal-matrix [2 3]))))
  (testing "rotation matrix"
    (is (v/approx= (v/of 1 2 3) 
                   (m/* (m/x-axis-rotation-matrix (* 2 Math/PI)) 
                        (v/of 1 2 3))))))

(deftest test-compose
  (testing "composing scales"
    (is (= (m/scale-matrix [3 6]) (m/* (m/scale-matrix [1 2]) (m/scale-matrix 2 3))))))

(deftest test-ops
  (testing "as-vector"
    (is (= (v/of 1 0 0 1) (m/as-vector (m/identity-matrix 2))))
    (is (= (v/of 1 0) (m/get-row (m/identity-matrix 2) 0)))))

(deftest test-constant-transform
  (testing "application"
    (is (= (v/of 1 0) (m/* (m/constant-transform (v/of 1 0)) (v/of 3 4))))
    (is (= (v/of 1 0) (m/* (m/constant-transform (v/of 1 0) :input-dimensions 3) (v/of 3 4 5))))))

(deftest test-get-set
  (testing "setting"
    (let [m (m/clone (m/identity-matrix 2))]
      (is (= 1.0 (m/get m 0 0)))
      (m/set m 0 0 2.0)
      (is (= 2.0 (m/get m 0 0))))))

(deftest test-arithmetic
  (testing "identity"
    (let [a (v/of 2 3)
          m (m/identity-matrix 2)
          r (m/* m a)]
      (is (= a r)))))

(deftest test-predicates
  (testing "fully mutable"
    (is (m/fully-mutable? (m/new-matrix 3 3)))
  )
  (testing "square"
    (is (m/square? (m/new-matrix 3 3)))
    (is (m/square? (m/identity-matrix 10)))
    (is (not (m/square? (m/new-matrix 4 3))))
  )
  (testing "identity"
    (is (m/identity? (m/identity-matrix 3)))
    (is (not (m/identity? (m/new-matrix 2 2 ))))
    (is (m/identity? (m/scale-matrix [1 1 1])))
    (is (not (m/identity? (m/scale-matrix [1 2 3]))))
  )
  (testing "zero"
    (is (m/zero? (m/new-matrix 2 3)))
    (is (m/zero? (m/scale-matrix [0 0 0 0 0])))
  )
  (testing "affine"
    (is (m/affine-transform? (m/new-matrix 2 3)))))

(deftest test-dimensions
  (testing "inputs"
    (is (= 3 (m/input-dimensions (m/new-matrix 2 3)))))
  (testing "outputs"
    (is (= 2 (m/output-dimensions (m/new-matrix 2 3)))))
  )