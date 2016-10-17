(ns mikera.vectorz.implementation-check
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)

(defn test-impls []
	(array [1])
	
	(def protos (utils/extract-protocols))
 
  (into {} (mapv #(do [% (utils/unimplemented %)])
                 [mikera.arrayz.INDArray
                  mikera.matrixx.AMatrix
                  mikera.vectorz.AVector
                  mikera.vectorz.AScalar]))
)