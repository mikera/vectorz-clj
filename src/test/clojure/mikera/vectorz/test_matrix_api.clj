(ns mikera.vectorz.test-matrix-api
  (:use [clojure test])
  (:use [core matrix])
  (:use core.matrix.operators)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(deftest test-ops
  (testing "addition"
    (is (= (v/of 1 2) (+ (v/of 1 1) [0 1])))
    (is (= [1.0 2.0] (+ [0 2] (v/of 1 0))))))
