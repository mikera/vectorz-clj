(ns mikera.vectorz.benchmark-matrix
  (:use core.matrix)
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3]))

(defn benchmarks []
  (let [^Vector3 a ( v/vec [1 2 3])
        ^Vector3 b ( v/vec [1 2 3])]
    (c/quick-bench (dotimes [i 1000] (.add a b))))) 