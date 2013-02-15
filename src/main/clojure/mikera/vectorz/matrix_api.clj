(ns mikera.vectorz.matrix-api
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector AScalar])
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
                (supports-dimensionality? [m dims] (or (== dims 0) (== dims 1) (== dims 2)))
                (new-vector [m length] (Vectorz/newVector (int length)))
                (new-matrix [m rows columns] (Matrixx/newMatrix (int rows) (int columns)))
                (new-matrix-nd [m shape] 
                               (case (count shape)
                                 0 0.0
                                 1 (Vectorz/newVector (int (first shape)))
                                 2 (Matrixx/newMatrix (int (first shape)) (int (second shape)))
                                 (error "Can't create vectorz matrix with dimensionality: " (count shape))))
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
  mikera.vectorz.Vector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] (.getArray m))
  mikera.vectorz.AVector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] nil)
  mikera.matrixx.AMatrix
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] nil)
  mikera.matrixx.MatrixMN
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] (.data m))) 

(extend-protocol mp/PDimensionInfo
   mikera.arrayz.INDArray
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
   mikera.vectorz.AScalar
    (dimensionality [m]
      0)
    (row-count [m]
      (error "Can't get row-count of a scalar"))
    (is-vector? [m]
      false)
    (is-scalar? [m]
      false)
    (column-count [m]
      (error "Can't get row-count of a scalar"))
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (error "Scalar does not have dimension: " x))
  mikera.vectorz.AVector
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
  mikera.matrixx.AMatrix
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
  mikera.vectorz.AVector
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (error "Can't access 2-dimensional index of a vector"))
    (get-nd [m indexes]
      (if-let [ni (next indexes)]
        (error "Can't access multi-dimensional index of a vector")
        (.get m (int (first indexes)))))
  mikera.matrixx.AMatrix
    (get-1d [m x]
      (.getRow m (int x)))
    (get-2d [m x y]
      (.get m (int x) (int y)))
    (get-nd [m indexes]
      (let [[x y & more] indexes]
        (if (seq more)
          (error "Can't get from AMatrix with more than 2 dimensions")
          (.get m (int x) (int y))))))

(extend-protocol mp/PZeroDimensionAccess
  mikera.vectorz.AScalar
    (get-0d [m]
      (.get m))
    (set-0d! [m value]
      (.set m (double value))))
    
(extend-protocol mp/PIndexedSetting
  AVector
    (set-1d [m row v] (.set m (int row) (double v)))
    (set-2d [m row column v] (error "Can't do 2-dimensional set on a 1D vector!"))
    (set-nd [m indexes v]
      (if (== 1 (count indexes))
        (.set m (int (first indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 1D vector!")))
    (is-mutable? [m] (.isFullyMutable m)) 
  AMatrix
    (set-1d [m row v] (error "Can't do 1-dimensional set on a 2D matrix!"))
    (set-2d [m row column v] (.set m (int row) (int column) (double v)))
    (set-nd [m indexes v]
      (if (== 2 (count indexes))
        (.set m (int (first indexes)) (int (second indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 2D matrix!")))
    (is-mutable? [m] (.isFullyMutable m)))


(extend-protocol mp/PMatrixSlices
  mikera.vectorz.AVector
    (get-row [m i]
      (.get m (int i)))
    (get-column [m i]
      (error "Can't access column of a 1D vector!"))
    (get-major-slice [m i]
      (.slice m (int i)))
    (get-slice [m dimension i]
      (if (== 0 i)
        (.get m (int i))
        (error "Can't get slice from vector with dimension: " dimension)))
  mikera.matrixx.AMatrix
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

(extend-protocol mp/PMatrixAdd
  mikera.vectorz.AScalar
    (matrix-add [m a]
      (+ (.get m) (double (coerce m a))))
    (matrix-sub [m a]
      (- (.get m) (double (coerce m a))))
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


(extend-protocol mp/PVectorOps
  mikera.vectorz.AVector
    (vector-dot [a b]
      (.dotProduct a (coerce a b)))
    (length-squared [a]
      (.magnitudeSquared a))
    (normalise [a]
      (v/normalise a)))
    
(defn vectorz-coerce [p]
  (let [dims (dimensionality p)]
    (cond
	    (== 0 dims)
        (cond 
          (number? p) (double p)
          (instance? AScalar p) p
          :else (double (mp/get-0d p)))
	    (or (instance? AVector p) (instance? AMatrix p)) 
	      p
	    (== 1 (dimensionality p))
	      (try (Vectorz/toVector p) (catch Throwable e nil))
	    (== 2 (dimensionality p))
	      (try (Matrixx/toMatrix p) (catch Throwable e nil))
	    :else nil)))

(extend-protocol mp/PCoercion
  mikera.vectorz.AVector
    (coerce-param [m param]
      (vectorz-coerce param))
  mikera.matrixx.AMatrix
    (coerce-param [m param]
      (vectorz-coerce param)))

(extend-protocol mp/PConversion
  mikera.vectorz.AVector
    (convert-to-nested-vectors [m]
      (into [] (seq m)))
  mikera.matrixx.AMatrix
    (convert-to-nested-vectors [m]
      (mapv #(into [] %) (mp/get-major-slice-seq m))))

(extend-protocol mp/PMatrixMultiply
  mikera.vectorz.AScalar
    (matrix-multiply [m a]
      (mp/pre-scale a (.get m)))
    (scale [m a]
      (* (.get m) a))
  mikera.vectorz.AVector
    (matrix-multiply [m a]
      (mp/matrix-multiply (mikera.matrixx.impl.ColumnMatrix/wrap m) a))
    (scale [m a]
      (v/scale m a))
  mikera.matrixx.AMatrix
    (matrix-multiply [m a]
      (if (instance? mikera.vectorz.AVector a)
        (.transform m ^AVector a)
        (m/* m (coerce m a))))
    (scale [m a]
      (m/scale m a)))

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

;; registration

(imp/register-implementation (v/of 0.0))