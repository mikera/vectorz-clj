(ns mikera.vectorz.matrix
  (:import [mikera.vectorz AVector Vectorz Vector Vector3])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:require mikera.vectorz.core)
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
  [rows cols]
  (Matrixx/newMatrix (int rows) (int cols)))

;; ============================================
;; Matrix application

(defn * 
  ([m a]))