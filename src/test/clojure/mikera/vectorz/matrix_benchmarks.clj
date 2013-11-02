(ns mikera.vectorz.matrix-benchmarks
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.stats)
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.matrix-api])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3 Vectorz])
  (:import [mikera.matrixx Matrixx]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(set-current-implementation :vectorz)

(defn benchmarks []
	;; elementwise mutation of 10x10 matrix, followed by computation of the sum
	;; => about 3,000 ns
	(defn buildsum [n]
	 (let [bv (zero-matrix n n)]
	   (dotimes [i n]
	     (dotimes [j n]
	       (mset! bv i j (* i j))))
	   (esum bv)))
	(c/quick-bench (buildsum 10))
	
	
	;; multiplication of two 100x100 matrices
	;; => about 1.3 ms
	(defn multiply [n]
	  (let [ma (zero-matrix n n)
	        mb (zero-matrix n n)]
	    (mmul ma mb)))
	(c/quick-bench (multiply 100))
	
	
	;; multiplication of two 10x10 matrices
	;; => about 1,800 ns
	(c/quick-bench (multiply 10))

)