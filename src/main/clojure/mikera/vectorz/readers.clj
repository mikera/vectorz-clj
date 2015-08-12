(ns mikera.vectorz.readers
  (:require [clojure.core.matrix :as m])
  (:import [mikera.vectorz AVector Vectorz Vector Vector1 Vector2 Vector3 Vector4 AScalar Scalar])
  (:import [mikera.arrayz INDArray Arrayz])
  (:import [mikera.matrixx AMatrix Matrixx])
  (:import [java.util List])
  (:require [mikera.cljutils.error :refer [error]]) 
  (:refer-clojure :exclude [vector]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn array 
  "Reads a data structure into a Vectorz array"
  (^INDArray [a]
    (Arrayz/create a)))

(defn vector 
  "Reads a data structure into a Vectorz vector"
  (^AVector [a]
    (cond
       (instance? List a) (Vectorz/create ^List a)
       (sequential? a) (Vectorz/create ^List (vec a))
       :else (error "Vector nust be read from a vector literal"))))

(defn matrix 
  "Reads a data structure into a Vectorz vector"
  (^AMatrix [a]
    (cond 
      (instance? List a) (Matrixx/create ^List a)
      (sequential? a) (Matrixx/create ^List (vec a))
      :else (error "Matrix nust be read as a vector of vectors"))))

(defn scalar 
  "Reads a data structure into a Vectorz vector"
  (^AScalar [a]
    (if (number? a)
      (Scalar/create (double a))
      (error "Scalar nust be read as a numerical value"))))

