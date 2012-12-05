(ns mikera.vectorz.test-matrix
  (:use clojure.test)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector]))


(deftest test-arithmetic
  (testing "identity"
    (let [a (v/of 2 3)
          m (m/identity-matrix 2)
          r (m/* m a)]
      (is (= a r)))))
