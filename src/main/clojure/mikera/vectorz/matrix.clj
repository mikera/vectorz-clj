(ns mikera.vectorz.matrix
  (:import [mikera.vectorz AVector Vectorz Vector Vector3])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:require [mikera.vectorz.core :as v])
  (:refer-clojure :exclude [*]))
  

;; ============================================
;; Core functions

(defn matrix?
  "Returns true if m is a matrix (i.e. an instance of mikera.matrixx.AMatrix)"
  ([m]
    (instance? mikera.matrixx.AMatrix m)))
  
;; ============================================
;; Matrix contructors

(defn new-matrix 
  ([rows cols]
    (Matrixx/newMatrix (int rows) (int cols))))

;; ============================================
;; matrix operations


;; ============================================
;; Matrix application

(defn * 
  ([^AMatrix m ^AVector a]
    (let [^AVector result (v/create-length (.outputDimenstions m))]
      (.transform m a result)
      result)))