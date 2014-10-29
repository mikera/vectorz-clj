(ns mikera.vectorz.test-sparse
  (:use [clojure test])
  (:use clojure.core.matrix)
  (:require clojure.core.matrix.compliance-tester)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.linear :as li])
  (:require [mikera.vectorz.matrix-api])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz Scalar])
  (:import [mikera.indexz AIndex Index])
  (:import [mikera.vectorz AVector Vectorz Vector])
  (:import [mikera.arrayz INDArray Array NDArray]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftest test-sparse-assign
  (let [pm [[6 7] [8 9]]
        sm (sparse :vectorz (matrix :vectorz [[1 2] [3 4]]))]
    (is (instance? mikera.arrayz.INDArray sm))
    (is (sparse? sm))
    (assign! sm pm)
    (is (== 30 (esum sm)))
    (assign! sm 2)
    (is (== 8 (esum sm)))))

(deftest test-sparse-assign-double
  (let [pm [[6 7] [8 9]]
        sm (sparse :vectorz pm)]
    (is (instance? mikera.arrayz.INDArray sm))
    (is (sparse? sm))
    (assign! sm 1)
    (is (== 4 (esum sm)))))

(deftest test-non-zero-indices
  (let [pm [[2 0] [0 1]]
        sm (sparse (matrix :vectorz pm))]
    (is (equals [0] (first (non-zero-indices sm))))
    (is (equals [1] (second (non-zero-indices sm))))))

(deftest test-ops
  (let [pm [[2 0] [0 1]]
        sm (sparse (matrix :vectorz pm))]
    (is (equals [[4 0] [ 0 2]] (div! sm 0.5)))
    (is (equals pm (mul! sm 0.5)))))