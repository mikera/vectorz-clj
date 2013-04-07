(ns mikera.vectorz.matrix-api
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz AVector Vectorz Vector AScalar Vector3])
  (:import [mikera.arrayz SliceArray INDArray])
  (:import [java.util List])
  (:import [mikera.transformz ATransform])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(eval
  `(extend-protocol mp/PImplementation
     ~@(mapcat 
         (fn [sym]
           (cons sym
             '(
                (implementation-key [m] :vectorz)
                (supports-dimensionality? [m dims] true)
                (new-vector [m length] (Vectorz/newVector (int length)))
                (new-matrix [m rows columns] (Matrixx/newMatrix (int rows) (int columns)))
                (new-matrix-nd [m shape] 
                               (case (count shape)
                                 0 0.0
                                 1 (Vectorz/newVector (int (first shape)))
                                 2 (Matrixx/newMatrix (int (first shape)) (int (second shape)))
                                 (SliceArray/create ^List (mapv (fn [_] (mp/new-matrix-nd m (next shape))) (range (first shape))))))
                (construct-matrix [m data]
                                  (cond 
                                    (mp/is-scalar? data) 
                                      (double data)
                                    (array? data) 
                                      (if (== 0 (mp/dimensionality data))
                                        (double (mp/get-0d data))
                                        (assign! (mp/new-matrix-nd m (shape data)) data))
                                    :default
                                      (let [vm (mp/construct-matrix [] data)] 
                                        ;; (println m vm (shape vm))
                                        (assign! (mp/new-matrix-nd m (shape vm)) vm)))))))
         ['mikera.vectorz.AVector 'mikera.matrixx.AMatrix 'mikera.vectorz.AScalar 'mikera.arrayz.INDArray]) ))



(extend-protocol mp/PDoubleArrayOutput
  mikera.vectorz.AScalar
    (to-double-array [m] (let [arr (double-array 1)] (aset arr (int 0) (.get m)) arr))
    (as-double-array [m] nil)
  mikera.vectorz.Vector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] (.getArray m))
  mikera.vectorz.AVector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] nil)
  mikera.matrixx.AMatrix
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] nil)
  mikera.matrixx.Matrix
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] (.data m))) 

(extend-protocol mp/PDimensionInfo
   INDArray
    (dimensionality [m]
      (.dimensionality m))
    (row-count [m]
      (mp/dimension-count m 0))
    (is-vector? [m]
      (== 1 (.dimensionality m)))
    (is-scalar? [m]
      false)
    (column-count [m]
      (mp/dimension-count m 1))
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (aget (.getShape m) (int x)))
  AScalar
    (dimensionality [m]
      0)
    (row-count [m]
      (error "Can't get row-count of a scalar"))
    (is-vector? [m]
      false)
    (is-scalar? [m]
      false) ;; this isn't an immutable scalar value in the core.matrix sense
    (column-count [m]
      (error "Can't get row-count of a scalar"))
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (error "Scalar does not have dimension: " x))
  AVector
    (dimensionality [m]
      1)
    (row-count [m]
      (.length m))
    (is-vector? [m]
      true)
    (is-scalar? [m]
      false)
    (column-count [m]
      1)
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (if (== x 0)
        (.length m)
        (error "Vector does not have dimension: " x)))
  AMatrix
    (dimensionality [m]
      2)
    (row-count [m]
      (.rowCount m))
    (is-vector? [m]
      false)
    (is-scalar? [m]
      false)
    (column-count [m]
      (.columnCount m))
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (cond 
        (== x 0) (.rowCount m)
        (== x 1) (.columnCount m)
        :else (error "Matrix does not have dimension: " x))))
    
(extend-protocol mp/PIndexedAccess
   INDArray
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (.get m (int x) (int y)))
    (get-nd [m indexes]
      (.get m (int-array indexes)))
   AScalar
    (get-1d [m x]
      (error "Can't access 1-dimensional index of a scalar"))
    (get-2d [m x y]
      (error "Can't access 2-dimensional index of a scalar"))
    (get-nd [m indexes]
      (if-let [ni (seq indexes)]
        (error "Can't access multi-dimensional index of a scalar")
        (.get m)))
  AVector
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (error "Can't access 2-dimensional index of a vector"))
    (get-nd [m indexes]
      (if-let [ni (next indexes)]
        (error "Can't access multi-dimensional index of a vector")
        (.get m (int (first indexes)))))
  AMatrix
    (get-1d [m x]
      (error "Can't access 1-dimensional index of a matrix"))
    (get-2d [m x y]
      (.get m (int x) (int y)))
    (get-nd [m indexes]
      (let [[x y & more] indexes]
        (if (seq more)
          (error "Can't get from AMatrix with more than 2 dimensions")
          (.get m (int x) (int y))))))

(extend-protocol mp/PZeroDimensionAccess
  INDArray
    (get-0d [m]
      (.get m))
    (set-0d! [m value]
      (.set m (double value)))
  AScalar
    (get-0d [m]
      (.get m))
    (set-0d! [m value]
      (.set m (double value))))

(extend-protocol mp/PIndexedSetting
  INDArray
    (set-1d [m row v] 
      (let [m (.clone m)] (.set m (int row) (double v)) m))
    (set-2d [m row column v] 
      (let [m (.clone m)] (.set m (int row) (int column) (double v)) m))
    (set-nd [m indexes v]
      (let [m (.clone m)] (.set m (int-array indexes) (double v)) m)) 
    (is-mutable? [m] (.isFullyMutable m)) 
  
  AScalar
    (set-1d [m row v] (error "Can't do 2-dimensional set on a 0-d array!"))
    (set-2d [m row column v] (error "Can't do 2-dimensional set on a 0-d array!"))
    (set-nd [m indexes v]
      (if (== 0 (count indexes))
        (let [^AScalar m (clone m)] (.set m (double v)) m)
        (error "Can't do " (count indexes) "-dimensional set on a 0-d array!"))) 
    (is-mutable? [m] (.isFullyMutable m)) 
  AVector
    (set-1d [m row v] 
      (let [m (.clone m)] (.set m (int row) (double v)) m))
    (set-2d [m row column v] (error "Can't do 2-dimensional set on a 1D vector!"))
    (set-nd [m indexes v]
      (if (== 1 (count indexes))
        (let [m (.clone m)] (.set m (int (first indexes)) (double v)) m)
        (error "Can't do " (count indexes) "-dimensional set on a 1D vector!"))) 
    (is-mutable? [m] (.isFullyMutable m)) 
  AMatrix
    (set-1d [m row v] (error "Can't do 1-dimensional set on a 2D matrix!"))
    (set-2d [m row column v] 
      (let [m (.clone m)] (.set m (int row) (int column) (double v))) m)
    (set-nd [m indexes v]
      (if (== 2 (count indexes))
        (let [m (.clone m)] (.set m (int (first indexes)) (int (second indexes)) (double v)))
        (error "Can't do " (count indexes) "-dimensional set on a 2D matrix!")))
    (is-mutable? [m] (.isFullyMutable m)))
    
(extend-protocol mp/PIndexedSettingMutable
  INDArray
    (set-1d! [m row v] 
      (.set m (int row) (double v)))
    (set-2d! [m row column v] 
      (.set m (int row) (int column) (double v)))
    (set-nd! [m indexes v]
      (.set m (int-array indexes) (double v))) 
  AScalar
    (set-1d! [m row v] (error "Can't do 1-dimensional set on a 0D array!"))
    (set-2d! [m row column v] (error "Can't do 1-dimensional set on a 0D array!"))
    (set-nd! [m indexes v]
      (if (== 0 (count indexes))
        (.set m (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 0D array!"))) 
  AVector
    (set-1d! [m row v] (.set m (int row) (double v)))
    (set-2d! [m row column v] (error "Can't do 2-dimensional set on a 1D vector!"))
    (set-nd! [m indexes v]
      (if (== 1 (count indexes))
        (.set m (int (first indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 1D vector!"))) 
  AMatrix
    (set-1d! [m row v] (error "Can't do 1-dimensional set on a 2D matrix!"))
    (set-2d! [m row column v] (.set m (int row) (int column) (double v)))
    (set-nd! [m indexes v]
      (if (== 2 (count indexes))
        (.set m (int (first indexes)) (int (second indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 2D matrix!"))))


(extend-protocol mp/PMatrixSlices
  AVector
    (get-row [m i]
      (.slice m (int i)))
    (get-column [m i]
      (error "Can't access column of a 1D vector!"))
    (get-major-slice [m i]
      (.slice m (int i)))
    (get-slice [m dimension i]
      (if (== 0 i)
        (.slice m (int i))
        (error "Can't get slice from vector with dimension: " dimension)))
  AMatrix
    (get-row [m i]
      (.getRow m (int i)))
    (get-column [m i]
      (.getColumn m (int i)))
    (get-major-slice [m i]
      (.slice m (int i)))
    (get-slice [m dimension i]
      (cond 
        (== 0 dimension) (.getRow m (int i))
        (== 1 dimension) (.getColumn m (int i))
        :else (error "Can't get slice from matrix with dimension: " dimension))))

(extend-protocol mp/PSliceView
  INDArray
    (get-major-slice-view [m i] 
      (.slice m (int i))))

(extend-protocol mp/PSliceSeq
  INDArray  
    (get-major-slice-seq [m] 
      (map #(.slice m (int %)) (range (aget (.getShape m) 0)))))

(extend-protocol mp/PSubVector
  mikera.vectorz.AVector
    (subvector [m start length]
      (.subVector m (int start) (int length)))) 

(extend-protocol mp/PSubMatrix
  mikera.vectorz.AVector
    (submatrix [m index-ranges]
      (let [[[start length]] index-ranges]
        (.subVector m (int start) (int length))))) 

(extend-protocol mp/PSummable
  mikera.vectorz.AVector
    (element-sum [m]
      (.elementSum m))
  mikera.matrixx.AMatrix
    (element-sum [m]
      (.elementSum m))
  mikera.vectorz.AScalar
    (element-sum [m]
      (.get m)))

(extend-protocol mp/PMatrixAdd
  mikera.vectorz.AScalar
    (matrix-add [m a]
      (+ (.get m) (double (mp/get-0d a))))
    (matrix-sub [m a]
      (- (.get m) (double (mp/get-0d a))))
  mikera.vectorz.AVector
    (matrix-add [m a]
      (v/add m ^AVector (coerce m a)))
    (matrix-sub [m a]
      (v/sub m ^AVector (coerce m a)))
  mikera.matrixx.AMatrix
    (matrix-add [m a]
      (let [m (m/clone m)] 
        (.add m (coerce m a))
        m))
    (matrix-sub [m a]
      (let [m (m/clone m)] 
        (.addMultiple m (coerce m a) -1.0)
        m)))

(extend-protocol mp/PMatrixAddMutable
  mikera.vectorz.AScalar
    (matrix-add! [m a]
      (+ (.get m) (double (mp/get-0d a))))
    (matrix-sub! [m a]
      (- (.get m) (double (mp/get-0d a))))
  mikera.vectorz.AVector
    (matrix-add! [m a]
      (.add m ^AVector (coerce m a)))
    (matrix-sub! [m a]
      (.sub m ^AVector (coerce m a)))
  mikera.matrixx.AMatrix
    (matrix-add! [m a]
      (.add m ^AMatrix (coerce m a)))
    (matrix-sub! [m a]
      (.sub m ^AMatrix (coerce m a))))

(extend-protocol mp/PVectorOps
  mikera.vectorz.AVector
    (vector-dot [a b]
      (.dotProduct a (coerce a b)))
    (length [a]
      (.magnitude a))
    (length-squared [a]
      (.magnitudeSquared a))
    (normalise [a]
      (v/normalise a)))

(extend-protocol mp/PMatrixOps
  AMatrix
    (trace [m]
      (.trace m))
    (determinant [m]
      (.determinant m))
    (inverse [m]
      (.inverse m))
    (negate [m]
      (let [m (.clone m)]
        (.scale m -1.0)
        m))
    (transpose [m]
      (.getTranspose m)))

(extend-protocol mp/PVectorCross
  mikera.vectorz.AVector
    (cross-product [a b]
      (let [v (Vector3. a)]
        (.crossProduct v ^AVector (mp/coerce-param a b))
        v))
    (cross-product! [a b]
      (.crossProduct a ^AVector (mp/coerce-param a b)))) 

(extend-protocol mp/PMatrixCloning
  INDArray
    (clone [m]
      (.clone m))
  AScalar 
    (clone [m]
      (.clone m))
  AVector
    (clone [m]
      (.clone m))
  AMatrix
	  (clone [m]
	    (.clone m)))
    
(defn vectorz-coerce 
  "Function to attempt conversion to Vectorz objects. May return nil if conversion fails."
  ([p]
	  (let [dims (dimensionality p)]
	    (cond
		    (instance? INDArray p) p
	      (== 0 dims)
	        (cond 
	          (number? p) (double p)
	          (instance? AScalar p) p
	          :else (double (mp/get-0d p)))
		    (== 1 (dimensionality p))
		      (try (Vectorz/toVector p) (catch Throwable e nil))
		    (== 2 (dimensionality p))
		      (try (Matrixx/toMatrix p) (catch Throwable e nil))
		    :else 
	        (let [^List sv (mapv (fn [sl] (vectorz-coerce sl)) (slices p))]
	          (and (seq sv) (sv 0) (SliceArray/create sv)))))))

(extend-protocol mp/PCoercion
  INDArray
    (coerce-param [m param]
      (vectorz-coerce param)))

(extend-protocol mp/PConversion
  AScalar
    (convert-to-nested-vectors [m]
      (.get m))  
  AVector
    (convert-to-nested-vectors [m]
      (into [] (seq m)))
  AMatrix
    (convert-to-nested-vectors [m]
      (mapv #(into [] %) (mp/get-major-slice-seq m))))

(extend-protocol mp/PMatrixMultiply
  mikera.vectorz.AScalar
    (matrix-multiply [m a]
      (mp/pre-scale a (.get m)))
  mikera.vectorz.AVector
    (matrix-multiply [m a]
      (mp/matrix-multiply (mikera.matrixx.impl.ColumnMatrix/wrap m) a))
  mikera.matrixx.AMatrix
    (matrix-multiply [m a]
      (if (instance? mikera.vectorz.AVector a)
        (.transform m ^AVector a)
        (m/* m (coerce m a)))))

(extend-protocol mp/PMatrixScaling
  mikera.vectorz.AVector
    (scale [m a]
      (let [m (.clone m)] 
        (.scale m (double a))
        m))
    (pre-scale [m a]
      (let [m (.clone m)] 
        (.scale m (double a))
        m))
  mikera.matrixx.AMatrix
    (scale [m a]
      (let [m (.clone m)] 
        (.scale m (double a))
        m))
    (pre-scale [m a]
      (let [m (.clone m)] 
        (.scale m (double a))
        m)))

(extend-protocol mp/PVectorTransform
  mikera.transformz.ATransform
    (vector-transform [m v] 
      (if (instance? AVector v) 
        (.transform m ^AVector v)
        (.transform m ^AVector (coerce m v))))
    (vector-transform! [m v] 
      (if (instance? AVector v) 
        (.transformInPlace m ^AVector v)
        (assign! v (transform m v)))))

;; TODO printing
;; we want to print in a form that can be constructed again
;;(defmethod print-method AVector [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))
;;(defmethod print-method AScalar [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))
;;(defmethod print-method AMatrix [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))

;; registration

(imp/register-implementation (v/of 0.0))