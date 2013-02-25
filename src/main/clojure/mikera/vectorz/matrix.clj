(ns mikera.vectorz.matrix
  (:import [mikera.vectorz AVector Vectorz Vector Vector3])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.transformz Transformz ATransform AAffineTransform])
  (:require [mikera.vectorz.core :as v])
  (:refer-clojure :exclude [* get set zero?]))
  
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ============================================
;; Core functions

(defn clone
  "Creates a (mutable) deep clone of a matrix. May not be exactly the same class as the original matrix."
  (^AMatrix [^AMatrix v]
    (.clone v)))

(defn transform?
  "Returns true if m is a transform (i.e. an instance of mikera.transformz.ATransform)"
  ([m]
    (instance? ATransform m)))

(defn affine-transform?
  "Returns true if m is a transform (i.e. an instance of mikera.transformz.AAffineTransform)"
  ([m]
    (instance? AAffineTransform m)))

(defn matrix?
  "Returns true if m is a matrix (i.e. an instance of mikera.matrixx.AMatrix)"
  ([m]
    (instance? AMatrix m)))

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
  "Gets a row of the matrix as a vector"
  ([^AMatrix m ^long row]
    (.getRow m (int row))))

;; ============================================
;; Matrix predicates

(defn fully-mutable?
  "Returns true if the matrix is fully mutable"
  ([^AMatrix m]
    (.isFullyMutable m)))

(defn zero?
  "Returns true if the matrix is a zero-filled matrix (i.e. maps every vector to zero)"
  ([^AMatrix m]
    (.isZeroMatrix m)))

(defn square?
  "Returns true if the matrix is a square matrix"
  ([^ATransform m]
    (.isSquare m)))

(defn identity?
  "Returns true if the matrix is an identity matrix"
  ([^ATransform m]
    (.isIdentity m)))


;; ============================================
;; General transform constructors

(defn constant-transform
  "Converts a vector to a constant transform"
  (^ATransform [^AVector v 
                & {:keys [input-dimensions]}]
    (mikera.transformz.Transformz/constantTransform (int (or input-dimensions (.length v))) v)))

;; ============================================
;; Matrix constructors

(defn new-matrix 
  "Creates a new, mutable, zero-filled matrix with the given number of rows and columns" 
  (^AMatrix [row-count column-count]
    (Matrixx/newMatrix (int row-count) (int column-count))))

(defn matrix
  "Creates a new, mutable matrix using the specified data, which should be a sequence of row vectors"
  ([rows]
    (let [vecs (vec (map v/vec rows))
          cc (apply max (map v/ecount vecs))
          rc (count rows)
          mat (new-matrix rc cc)]
      (dotimes [i rc]
        (let [^AVector v (vecs i)
              ^AVector row (get-row mat i)]
          (.copyTo v row (int 0))))
      mat)))

(defn identity-matrix
  "Returns an immutable identity matrix for the given number of dimensions."
  (^AMatrix [dimensions]
    (Matrixx/createIdentityMatrix (int dimensions))))

(defn diagonal-matrix
  "Creates a diagonal matrix, using the sequence of diagonal values provided" 
  (^AMatrix [diagonal-values]
    (mikera.matrixx.impl.DiagonalMatrix/create (double-array diagonal-values))))

(defn scale-matrix
  "Creates a diagonal scaling matrix" 
  (^AMatrix [scale-factors]
    (Matrixx/createScaleMatrix (double-array (seq scale-factors))))
  (^AMatrix [dimensions factor]
    (Matrixx/createScaleMatrix (int dimensions) (double factor))))

(defn scalar-matrix 
  "Creates a dioagonal scalar matrix (multiplies all components by same factor)"
  (^AMatrix [dimensions factor]
    (Matrixx/createScalarMatrix (int dimensions) (double factor))))

(defn x-axis-rotation-matrix
  "Creates a rotation matrix with the given number of radians around the x axis"
  (^AMatrix [angle]
    (Matrixx/createXAxisRotationMatrix (double angle))))

(defn y-axis-rotation-matrix
  "Creates a rotation matrix with the given number of radians around the y axis"
  (^AMatrix [angle]
    (Matrixx/createYAxisRotationMatrix (double angle))))

(defn z-axis-rotation-matrix
  "Creates a rotation matrix with the given number of radians around the z axis"
  (^AMatrix [angle]
    (Matrixx/createZAxisRotationMatrix (double angle))))

;; ============================================
;; matrix operations

(defn scale
  "Scales a matrix by a scalar factor"
  (^AMatrix [^AMatrix m factor]
    (let [^AMatrix m (clone m)]
      (.addMultiple m m (- (double factor) 1.0))
      m)))

(defn input-dimensions 
  "Gets the number of input dimensions (columns) of a matrix or other transform"
  (^long [^ATransform m]
    (.inputDimensions m)))

(defn output-dimensions 
  "Gets the number of output dimensions (rows) of a matrix or other transform"
  (^long [^ATransform m]
    (.outputDimensions m)))

(defn transpose!
  "Transposes a matrix in place, if possible"
  (^AMatrix [^AMatrix m]
    (.transposeInPlace m)
    m))

(defn transpose
  "Gets the transpose of a matrix as a transposed reference to the original matrix"
  (^AMatrix [^AMatrix m]
    (.getTranspose m)))

(defn as-vector
  "Treats a Matrix as a vector reference (in row major order)"
  (^AVector [^AMatrix m]
    (.asVector m)))

(defn inverse
  "Gets the inverse of a square matrix as a new matrix."
  (^AMatrix [^AMatrix m]
    (.inverse m)))

(defn compose!
  "Composes a transform with another transform (in-place). Second transform should be square."
  (^ATransform [^ATransform a ^ATransform b]
    (.composeWith a b)
    a))

(defn compose
  "Composes a transform with another transform"
  (^ATransform [^ATransform a ^ATransform b]
    (.compose a b)))

(defn determinant
  "Gets the determinant of a (square) matrix"
  (^double [^AMatrix m]
    (.determinant m)))



;; ============================================
;; Matrix application

(defn transform! 
  "Applies a matrix transform to a vector, modifying the vector in place"
  (^AVector [^ATransform m ^AVector a]
    (.transformInPlace m a)
    a))

(defn transform 
  "Applies a matrix transform to a vector, returning a new vector"
  (^AVector [^ATransform m ^AVector a]
    (let [^AVector result (v/create-length (.outputDimensions m))]
      (.transform m a result)
      result)))

(defn transform-normal 
  "Applies a an affine transform to a normal vector, storing the result in dest"
  (^AVector [^AAffineTransform m ^AVector src ^AVector dest]
    (.transformNormal m src dest)))

(defn *
  "Applies a matrix to a vector or matrix, returning a new vector or matrix. If applied to a vector, the vector is transformed. If applied to a matrix, the two matrices are composed"
  ([^ATransform m a]
    (if (instance? AVector a)
      (transform m a)
      (compose m a))))