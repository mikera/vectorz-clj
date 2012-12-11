(ns mikera.vectorz.test-matrix
  (:use clojure.test)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(deftest test-constructors
  (testing "identity"
    (is (= (m/matrix [[1 0] [0 1]]) (m/identity-matrix 2)))
    (is (= (m/identity-matrix 3) (m/scale-matrix [1 1 1]))))
  (testing "scale matrix"
    (is (= (m/matrix [[1 0] [0 1]]) (m/scale-matrix 2 1))))
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
