(ns mikera.vectorz.matrix
  (:import [mikera.vectorz AVector Vectorz Vector Vector3])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:require [mikera.vectorz.core :as v])
  (:refer-clojure :exclude [* get set]))
  

;; ============================================
;; Core functions

(defn clone
  "Creates a (mutable) clone of a matrix. May not be exactly the same class as the original matrix."
  (^AMatrix [^AMatrix v]
    (.clone v)))

(defn matrix?
  "Returns true if m is a matrix (i.e. an instance of mikera.matrixx.AMatrix)"
  ([m]
    (instance? mikera.matrixx.AMatrix m)))

(defn get
  "Returns the component of a matrix at a specific (row,column) position"
  (^double [^AMatrix m ^long row ^long column]
    (.get m (int row) (int column))))

(defn set
  "Sets the component of a matrix at a (row,column) position (mutates in place)"
  ([^AMatrix m ^long row ^long column ^double value]
    (.set m (int row) (int column) value)
    m))

(defn get-row
  "Sets the component of a matrix at a (row,column) position (mutates in place)"
  ([^AMatrix m ^long row]
    (.getRow m (int row))))
  
;; ============================================
;; Matrix contructors

(defn new-matrix 
  (^AMatrix [rows cols]
    (Matrixx/newMatrix (int rows) (int cols))))

(defn matrix
  "Creates a new matrix using the specified data, which should be a sequence of row vectors"
  ([rows]
    (let [vecs (vec (map v/vec rows))
          cc (apply max (map v/length vecs))
          rc (count rows)
          mat (new-matrix rc cc)]
      (dotimes [i rc]
        (let [^AVector v (vecs i)]
          (.copyTo v (get-row mat i) 0)))
      mat)))

(defn identity-matrix
  (^AMatrix [dimensions]
    (Matrixx/createIdentityMatrix (int dimensions))))

;; ============================================
;; matrix operations

(defn transpose!
  "Transposes a matrix in place, if possible"
  (^AMatrix [^AMatrix m]
    (.transposeInPlace m)
    m))

(defn as-vector
  "Treats a Matrix as a vector reference (in row major order)"
  (^AVector [^AMatrix m]
    (.asVector m)))

(defn transpose
  "Gets the transpose of a matrix as a transposed reference to the original matrix"
  (^AMatrix [^AMatrix m]
    (.getTranspose m)))

(defn inverse
  "Gets the inverse of a square matrix as a new matrix."
  (^AMatrix [^AMatrix m]
    (.getInverse m)))

;; ============================================
;; Matrix application

(defn apply! 
  "Applies a matrix to a vector, modifying the vector in place"
  ([^AMatrix m ^AVector a]
    (.transformInPlace m a)))

(defn * 
  "Applies a matrix to a vector, returning a new vector"
  ([^AMatrix m ^AVector a]
    (let [^AVector result (v/create-length (.outputDimensions m))]
      (.transform m a result)
      result)))