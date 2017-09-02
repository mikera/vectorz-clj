(ns mikera.vectorz.implementation-check
  (:use clojure.core.matrix)
  (:require [clojure.test :as test])
  (:require [clojure.core.matrix.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)

(def protos (utils/extract-protocols))

(defn test-impls 
  "Gets a map of vectorz types to unimplemented interfaces.
   Intended to allow us to check which protocols still need to be implemented."
  ([]
    (array :vectorz [1])
    (into {} (mapv #(do [% (utils/unimplemented %)])
                   [mikera.arrayz.INDArray
                    mikera.matrixx.AMatrix
                    mikera.vectorz.AVector
                    mikera.vectorz.AScalar]))))

