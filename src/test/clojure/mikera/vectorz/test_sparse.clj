(ns mikera.vectorz.test-sparse
  (:use [clojure test])
  (:use clojure.core.matrix)
  (:require clojure.core.matrix.compliance-tester)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.linear :as li])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz Scalar])
  (:import [mikera.indexz AIndex Index])
  (:import [mikera.vectorz AVector Vectorz Vector])
  (:import [mikera.arrayz INDArray Array NDArray]))

(deftest test-sparse
  (is (instance? mikera.arrayz.INDArray (sparse (matrix :vectorz [[1 2] [3 4]])))))