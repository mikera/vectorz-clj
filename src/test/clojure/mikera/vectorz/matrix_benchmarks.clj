(ns mikera.vectorz.matrix-benchmarks
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.stats)
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.matrix-api])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3 Vectorz]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(set-current-implementation :vectorz)

(defn buildsum [n]
 (let [bv (zero-vector (* n n))
       res (sinh bv)]))

(c/quick-bench (buildsum 30))