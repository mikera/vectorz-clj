(ns mikera.vectorz.implementation-check
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)

(defn test-impls []
	(array [1])
	
	(def protos (utils/extract-protocols))
 
  (utils/unimplemented mikera.arrayz.INDArray) 
  (utils/unimplemented mikera.matrixx.AMatrix) 
  (utils/unimplemented mikera.vectorz.AVector)
  (utils/unimplemented mikera.vectorz.AScalar)
)