(ns mikera.vectorz.matrix-api
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.matrixx.impl DiagonalMatrix])
  (:import [mikera.vectorz AVector Vectorz Vector AScalar Vector3 Ops])
  (:import [mikera.vectorz.impl DoubleScalar])
  (:import [mikera.arrayz Arrayz SliceArray INDArray])
  (:import [java.util List])
  (:import [mikera.transformz ATransform])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare vectorz-coerce* avector-coerce*)

(defmacro tag-symbol [tag form]
  (let [tagged-sym (vary-meta (gensym "res") assoc :tag tag)]
    `(let [~tagged-sym ~form] ~tagged-sym)))

(defmacro vectorz-coerce [x]
  `(tag-symbol mikera.arrayz.INDArray
               (let [x# ~x]
                 (if (instance? INDArray x#) x# (vectorz-coerce* x#)))))

(defmacro avector-coerce 
  "Coerces an argument x to an AVector instance, of same size as m"
  ([m x]
    `(tag-symbol mikera.vectorz.AVector
                 (let [x# ~x] 
                   (if (instance? AVector x#) x# (avector-coerce* ~m x#)))))
  ([x]
    `(tag-symbol mikera.vectorz.AVector
                 (let [x# ~x] 
                   (if (instance? AVector x#) x# (avector-coerce* x#))))))

(defmacro with-clone [[sym exp] & body]
  (let []
    (when-not (symbol? sym) (error "Symbol required for with-clone binding"))
    `(let [~sym (.clone ~(if exp exp sym))]
       ~@body
       ~sym)))

(defn avector-coerce* 
  (^AVector [^AVector v m]
	  (cond
      (number? m) 
        (let [r (Vectorz/newVector (.length v))] (.fill r (double m)) r)
	    (instance? AVector m) m
      (== (mp/dimensionality m) 1)
        (let [len (.length v)
              r (Vectorz/newVector len)]
          (assign! r m) r)
      :else (Vectorz/toVector m)))
  ([m]
    (cond
      (number? m) 
        (let [r (Vectorz/newVector 1)] (.fill r (double m)) r)
	    (instance? AVector m) m
      (== (mp/dimensionality m) 1)
        (let [len (ecount m)
              r (Vectorz/newVector len)]
          (assign! r m) r)
      :else (Vectorz/toVector m)))) 

    
(defn vectorz-coerce* 
  "Function to attempt conversion to Vectorz objects. May return nil if conversion fails."
  (^INDArray [p]
	  (let [dims (dimensionality p)]
	    (cond
		    (instance? INDArray p) p
	      (== 0 dims)
	        (cond 
	          (number? p) (DoubleScalar. (.doubleValue ^Number p))
	          (instance? AScalar p) p
            (nil? p) nil
	          :else (do
                   ;; (println (str "Coercing " p))
                   (DoubleScalar. (double (mp/get-0d p)))))
		    (== 1 dims)
		      (try (Vectorz/toVector (mp/convert-to-nested-vectors p)) (catch Throwable e nil))
		    (== 2 dims)
		      (try (Matrixx/toMatrix (mp/convert-to-nested-vectors p)) (catch Throwable e nil))
		    :else 
	        (let [^List sv (mapv (fn [sl] (vectorz-coerce sl)) (slices p))]
	          (and (seq sv) (sv 0) (SliceArray/create sv)))))))


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
                                    (instance? INDArray data)
                                      (.clone ^INDArray data)
                                    (mp/is-scalar? data) 
                                      (double data)
                                    (array? data) 
                                      (if (== 0 (mp/dimensionality data))
                                        (double (mp/get-0d data))
                                        (vectorz-coerce data))
                                    :default
                                      (let [vm (mp/construct-matrix [] data)] 
                                        ;; (println m vm (shape vm))
                                        (assign! (mp/new-matrix-nd m (shape vm)) vm)))))))
         ['mikera.vectorz.AVector 'mikera.matrixx.AMatrix 'mikera.vectorz.AScalar 'mikera.arrayz.INDArray]) ))

(extend-protocol mp/PTypeInfo
  INDArray
    (element-type [m] (Double/TYPE)))

(extend-protocol mp/PMutableMatrixConstruction
  INDArray
    (mutable-matrix [m] (.clone m))) 

(extend-protocol mp/PMatrixMutableScaling
  INDArray
    (scale! [m a]
      (.scale m (double (mp/get-0d a))))
    (pre-scale! [m a]
      (.scale m (double (mp/get-0d a))))
  AVector
    (scale! [m a]
      (.scale m (double (mp/get-0d a))))
    (pre-scale! [m a]
      (.scale m (double (mp/get-0d a)))))

(extend-protocol mp/PDoubleArrayOutput
  INDArray
    (to-double-array [m] 
      (let [arr (double-array (.elementCount m))] 
        (.getElements m arr (int 0))
        arr))
    (as-double-array [m] nil)
  AScalar
    (to-double-array [m] (let [arr (double-array 1)] (aset arr (int 0) (.get m)) arr))
    (as-double-array [m] nil)
  Vector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] (.getArray m))
  AVector
    (to-double-array [m] (.toArray m))
    (as-double-array [m] nil)
  AMatrix
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] nil)
  Matrix
    (to-double-array [m] (.toArray (.asVector m)))
    (as-double-array [m] (.data m))) 

(extend-protocol mp/PVectorisable
  INDArray
    (to-vector [m]
      (.toVector m))
  AVector
    (to-vector [m]
      (.clone m)))

; TODO: wait for next core.matrix after 0.8.0
(extend-protocol mp/PMutableFill
  INDArray
  (fill!
    [m value]
    (.fill m (double (mp/get-0d value)))))

(extend-protocol mp/PDimensionInfo
   INDArray
    (dimensionality [m]
      (.dimensionality m))
    (is-vector? [m]
      (== 1 (.dimensionality m)))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (.getShape m (int x)))
  AScalar
    (dimensionality [m]
      0)
    (is-vector? [m]
      false)
    (is-scalar? [m]
      false) ;; this isn't an immutable scalar value in the core.matrix sense
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (error "Scalar does not have dimension: " x))
  AVector
    (dimensionality [m]
      1)
    (is-vector? [m]
      true)
    (is-scalar? [m]
      false)
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (if (== x 0)
        (.length m)
        (error "Vector does not have dimension: " x)))
  AMatrix
    (dimensionality [m]
      2)
    (is-vector? [m]
      false)
    (is-scalar? [m]
      false)
    (get-shape [m]
      (.getShape m))
    (dimension-count [m x]
      (let [x (int x)]
        (cond 
          (== x 0) (.rowCount m)
          (== x 1) (.columnCount m)
          :else (error "Matrix does not have dimension: " x)))))
    
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

(extend-protocol mp/PSpecialisedConstructors
  INDArray
    (identity-matrix [m dims] 
      (Matrixx/createIdentityMatrix (int dims)))
    (diagonal-matrix [m diagonal-values] 
      (DiagonalMatrix/create (Vectorz/toVector diagonal-values))))

(extend-protocol mp/PBroadcast
  INDArray 
    (broadcast [m target-shape]
     (.broadcast m (int-array target-shape)))) 

(extend-protocol mp/PReshaping
  INDArray 
    (reshape [m target-shape]
      (.reshape m (int-array target-shape)))) 

(extend-protocol mp/PIndexedSetting
  INDArray
    (set-1d [m row v] 
      (with-clone [m] (.set m (int row) (double v))))
    (set-2d [m row column v] 
      (with-clone [m] (.set m (int row) (int column) (double v))))
    (set-nd [m indexes v]
      (with-clone [m] (.set m (int-array indexes) (double v)))) 
    (is-mutable? [m] (.isFullyMutable m)) 
  
  AScalar
    (set-1d [m row v] (error "Can't do 2-dimensional set on a 0-d array!"))
    (set-2d [m row column v] (error "Can't do 2-dimensional set on a 0-d array!"))
    (set-nd [m indexes v]
      (if (== 0 (count indexes))
        (DoubleScalar/create (double v))
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
  INDArray
    (get-row [m i]
      (if (== 2 (mp/dimensionality m))
        (.slice m (int i))
        (error "Can't get row of array with dimensionslty: " (mp/dimensionality m))))
    (get-column [m i]
      (if (== 2 (mp/dimensionality m))
        (.slice m (int 1) (int i))
        (error "Can't get column of array with dimensionslty: " (mp/dimensionality m))))
    (get-major-slice [m i]
      (.slice m (int i)))
    (get-slice [m dimension i]
      (let [dimension (int dimension)]
        (.slice m dimension (int i))))
  AVector
    (get-row [m i]
      (error "Can't access row of a 1D vector!"))
    (get-column [m i]
      (error "Can't access column of a 1D vector!"))
    (get-major-slice [m i]
      (.slice m (int i)))
    (get-slice [m dimension i]
      (if (== 0 dimension)
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
      (let [dimensions (int dimension)]
        (cond 
          (== 0 dimension) (.getRow m (int i))
          (== 1 dimension) (.getColumn m (int i))
          :else (error "Can't get slice from matrix with dimension: " dimension)))))

(extend-protocol mp/PSliceView
  INDArray
    (get-major-slice-view [m i] 
      (.slice m (int i))))

(extend-protocol mp/PSliceSeq
  INDArray  
    (get-major-slice-seq [m] 
      (seq (.getSliceViews m))))

(extend-protocol mp/PMatrixSubComponents
  AMatrix
    (main-diagonal [m]
      (.getLeadingDiagonal m)))

(extend-protocol mp/PAssignment
  AScalar
    (assign! 
      [m source] (.set m (double (mp/get-0d source))))
    (assign-array! 
      ([m arr] (.set m (double (nth arr 0))))
      ([m arr start length] (.set m (double (nth arr 0)))))
  AVector
    (assign! [m source] 
      (cond 
        (number? source) 
           (.fill m (double source))
        (instance? INDArray source) 
           (.set m ^INDArray source)
        (== 0 (mp/dimensionality source))
           (.fill m (mp/get-0d source))
        :else 
           (dotimes [i (.length m)]
             (.set m i (double (mp/get-1d source i))))))
    (assign-array! 
      ([m arr] (dotimes [i (count arr)] (.set m (int i) (double (nth arr i)))))
      ([m arr start length] 
        (let [length (long length) start (long start)] 
          (dotimes [i length] (.set m (int i) (double (nth arr (+ i start))))))))
    
  INDArray
    (assign! [m source] 
      (.set m (vectorz-coerce source)))
    (assign-array!
      ([m arr]
	      (let [alen (long (count arr))]
	        (if (mp/is-vector? m)
	          (dotimes [i alen]
	            (mp/set-1d! m i (nth arr i)))
	          (mp/assign-array! m arr 0 alen))))
      ([m arr start length]
	      (let [length (long length)
              start (long start)]
         (if (mp/is-vector? m)
	          (dotimes [i length]
	            (mp/set-1d! m i (nth arr (+ start i))))
	          (let [ss (seq (mp/get-major-slice-seq m))
	                skip (long (if ss (mp/element-count (first (mp/get-major-slice-seq m))) 0))]
	            (doseq-indexed [s ss i]
	              (mp/assign-array! s arr (+ start (* skip i)) skip))))))))

(extend-protocol mp/PSubVector
  AVector
    (subvector [m start length]
      (.subVector m (int start) (int length)))) 

(extend-protocol mp/PSubMatrix
  AVector
    (submatrix [m index-ranges]
      (let [[[start length]] index-ranges]
        (.subVector m (int start) (int length))))) 

(extend-protocol mp/PSummable
  AVector
    (element-sum [m]
      (.elementSum m))
  AMatrix
    (element-sum [m]
      (.elementSum m))
  AScalar
    (element-sum [m]
      (.get m))
  INDArray 
    (element-sum [m]
      (.elementSum m)))

(extend-protocol mp/PMatrixAdd
  mikera.vectorz.AScalar
    (matrix-add [m a]
      (+ (.get m) (double (mp/get-0d a))))
    (matrix-sub [m a]
      (- (.get m) (double (mp/get-0d a))))
  mikera.vectorz.AVector
    (matrix-add [m a]
      (let [m (.clone m)
            ^AVector a (vectorz-coerce a)] 
        (.add m a) m))
    (matrix-sub [m a]
      (let [m (.clone m)
            ^AVector a (vectorz-coerce a)] 
        (.sub m a) m))
  mikera.matrixx.AMatrix
    (matrix-add [m a]
      (let [^AMatrix m (.clone m)
            ^INDArray a (vectorz-coerce a)] 
        (.add m a)
        m))
    (matrix-sub [m a]
      (let [m (.clone m)] 
        (.sub m (vectorz-coerce a))
        m)))

(extend-protocol mp/PMatrixAddMutable
  INDArray
    (matrix-add! [m a]
      (.add m (vectorz-coerce a)))
    (matrix-sub! [m a]
      (.sub m (vectorz-coerce a)))
  AScalar
    (matrix-add! [m a]
      (.add m (double (mp/get-0d a))))
    (matrix-sub! [m a]
      (.sub m (double (mp/get-0d a))))
  AVector
    (matrix-add! [m a]
      (.add m (avector-coerce m a)))
    (matrix-sub! [m a]
      (.sub m (avector-coerce m a)))
  AMatrix
    (matrix-add! [m a]
      (.add m ^AMatrix (coerce m a)))
    (matrix-sub! [m a]
      (.sub m ^AMatrix (coerce m a))))

(extend-protocol mp/PVectorOps
  AVector
    (vector-dot [a b]
      (.dotProduct a (coerce a b)))
    (length [a]
      (.magnitude a))
    (length-squared [a]
      (.magnitudeSquared a))
    (normalise [a]
      (let [a (.clone a)] 
        (.normalise a)
        a)))

(extend-protocol mp/PMatrixOps
  AMatrix
    (trace [m]
      (.trace m))
    (determinant [m]
      (.determinant m))
    (inverse [m]
      (.inverse m)))

(extend-protocol mp/PNegation
  AScalar (negate [m] (with-clone [m] (.negate m)))
  AVector (negate [m] (with-clone [m] (.negate m)))
  AMatrix (negate [m] (with-clone [m] (.negate m)))
  INDArray (negate [m] (with-clone [m] (.negate m))))

(extend-protocol mp/PTranspose
  INDArray (transpose [m] (.getTranspose m))
  AScalar (transpose [m] m)
  AVector (transpose [m] m)
  AMatrix (transpose [m] (.getTranspose m))) 

(extend-protocol mp/PVectorCross
  AVector
    (cross-product [a b]
      (let [v (Vector3. a)]
        (.crossProduct v ^AVector (mp/coerce-param a b))
        v))
    (cross-product! [a b]
      (.crossProduct a ^AVector (mp/coerce-param a b)))) 

(extend-protocol mp/PMatrixCloning
  INDArray (clone [m] (.clone m))
  AScalar (clone [m] (.clone m))
  AVector (clone [m] (.clone m))
  AMatrix	(clone [m] (.clone m)))

(extend-protocol mp/PCoercion
  INDArray
    (coerce-param [m param]
      (if (number? param)
        param
        (vectorz-coerce param))))

(extend-protocol mp/PConversion
  AScalar
    (convert-to-nested-vectors [m]
      (.get m))  
  AVector
    (convert-to-nested-vectors [m]
      (into [] (seq m)))
  AMatrix
    (convert-to-nested-vectors [m]
      (mapv #(into [] %) (mp/get-major-slice-seq m)))
  INDArray
    (convert-to-nested-vectors [m]
      (if (== 0 (.dimensionality m))
        (mp/get-0d m)
        (mapv mp/convert-to-nested-vectors (.getSlices m)))))

(extend-protocol mp/PMatrixMultiply
  AScalar
    (matrix-multiply [m a]
      (mp/pre-scale a (.get m)))
    (element-multiply [m a]
      (mp/pre-scale a (.get m)))
  AVector
    (matrix-multiply [m a]
      (.innerProduct m (vectorz-coerce a)))
    (element-multiply [m a]
      (with-clone [m] (.multiply m (vectorz-coerce a))))
  AMatrix
    (matrix-multiply [m a]
      (cond 
        (instance? AVector a) 
          (let [^AVector r (Vectorz/newVector (.rowCount m))] 
            (.transform m ^AVector a r)
            r)
        (instance? AMatrix a) (.compose m ^AMatrix a) 
        :else (.innerProduct m (vectorz-coerce a))))
    (element-multiply [m a]
      (with-clone [m] 
        (.multiply m (vectorz-coerce a))))
  INDArray
    (matrix-multiply [m a]
      (if-let [^INDArray a (vectorz-coerce a)]
        (.innerProduct m a)
        (error "Can't convert to vectorz representation: " a)))
    (element-multiply [m a]
      (with-clone [m] 
        (.multiply m (vectorz-coerce a)))))

(extend-protocol mp/PMatrixProducts
  INDArray
    (inner-product [m a] (.innerProduct m (vectorz-coerce a)))
    (outer-product [m a] (.outerProduct m (vectorz-coerce a))))

(defn vectorz-scale 
  "Scales a vectorz array, return a new scaled array"
  ([^INDArray m ^double a]
    (with-clone [m] (.scale m (double a)))))

(extend-protocol mp/PAddProduct
  AVector
    (add-product [m a b]
      (with-clone [m]
        (.addProduct m (avector-coerce m a) (avector-coerce m b))))) 

(extend-protocol mp/PAddProductMutable
  AVector
    (add-product! [m a b]
      (.addProduct m (avector-coerce m a) (avector-coerce m b)))) 

(extend-protocol mp/PAddScaled
  AVector
    (add-scaled [m a factor]
      (with-clone [m] 
        (.addMultiple m (avector-coerce m a) (double factor))))) 

(extend-protocol mp/PAddScaledMutable
  AVector
    (add-scaled! [m a factor]
      (.addMultiple m (avector-coerce m a) (double factor)))) 

(extend-protocol mp/PAddScaledProduct
  AVector
    (add-scaled-product [m a b factor]
      (with-clone [m]
        (.addProduct m (avector-coerce m a) (avector-coerce m b) (double factor))))) 

(extend-protocol mp/PAddScaledProductMutable
  AVector
    (add-scaled-product! [m a b factor]
      (.addProduct m (avector-coerce m a) (avector-coerce m b) (double factor)))) 

(extend-protocol mp/PMatrixScaling
  AScalar 
    (scale [m a] (vectorz-scale m (double a)))
    (pre-scale [m a] (vectorz-scale m (double a)))
  AVector
    (scale [m a] (vectorz-scale m (double a)))
    (pre-scale [m a] (vectorz-scale m (double a)))
  AMatrix
    (scale [m a] (vectorz-scale m (double a)))
    (pre-scale [m a] (vectorz-scale m (double a)))
  INDArray
    (scale [m a] (vectorz-scale m (double a)))
    (pre-scale [m a] (vectorz-scale m (double a))))

(extend-protocol mp/PMatrixAdd
  AVector
    (matrix-add [m a] (with-clone [m] (.add m (vectorz-coerce a))))
    (matrix-sub [m a] (with-clone [m] (.sub m (vectorz-coerce a)))))

(extend-protocol mp/PMatrixAddMutable
  AVector
    (matrix-add! [m a] (.add m (vectorz-coerce a)) m)
    (matrix-sub! [m a] (.sub m (vectorz-coerce a)) m))

(extend-protocol mp/PVectorTransform
  ATransform
    (vector-transform [m v] 
      (if (instance? AVector v) 
        (.transform m ^AVector v)
        (.transform m ^AVector (vectorz-coerce v))))
    (vector-transform! [m v] 
      (if (instance? AVector v) 
        (.transformInPlace m ^AVector v)
        (assign! v (transform m v)))))

(extend-protocol mp/PMutableVectorOps
  AVector
    (normalise! [a]
      (.normalise a)))

(extend-protocol mp/PMatrixOps
  AMatrix
    (trace [m]
      (.trace m))
    (determinant [m]
      (.determinant m))
    (inverse [m]
      (.inverse m)))

(extend-protocol mp/PSquare
  INDArray
    (square [m]
      (with-clone [m] (.square m)))
  AVector
    (square [m]
      (with-clone [m] (.square m))))

(extend-protocol mp/PElementCount
  INDArray
    (element-count [m]
      (.elementCount m))
  AMatrix
    (element-count [m]
      (.elementCount m))
  AVector
    (element-count [m]
      (.elementCount m))
  AScalar 
    (element-count [m]
      1))

(extend-protocol mp/PSliceJoin
  AVector
    (join [m a] 
          (.join m (avector-coerce a))))

(extend-protocol mp/PVectorView
  INDArray
    (as-vector [m]
      (.asVector m))
  AVector
    (as-vector [m]
      m))

(extend-protocol mp/PFunctionalOperations
  INDArray
  (element-seq
    [m]
    (let [ec (.elementCount m)
          ^doubles data (double-array ec)]
      (.getElements m data (int 0))
      (seq data)))
  (element-map
    ([m f]
      (let [ec (.elementCount m)
            ^doubles data (double-array ec)
            ^ints sh (.getShape m)]
        (.getElements m data (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i))))) 
        (Arrayz/createFromVector (Vector/wrap data) sh)))
    ([m f a]
      (let [ec (.elementCount m)
            a (vectorz-coerce a) 
            ^doubles data (double-array ec)
            ^doubles data2 (double-array ec)
            ^ints sh (.getShape m)]
        (when-not (== ec (.elementCount a)) (error "Arrays do do have matching number of elements")) 
        (.getElements m data (int 0))
        (.getElements a data2 (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i) (aget data2 i))))) 
        (Arrayz/createFromVector (Vector/wrap data) sh)))
    ([m f a more]
      (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f a more))))
  (element-map!
    ([m f]
      (let [ec (.elementCount m)
            ^doubles data (double-array ec)
            ^ints sh (.getShape m)]
        (.getElements m data (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i))))) 
        (.setElements m data)))
    ([m f a]
      (let [ec (.elementCount m)
            a (vectorz-coerce a) 
            ^doubles data (double-array ec)
            ^doubles data2 (double-array ec)
            ^ints sh (.getShape m)]
        (when-not (== ec (.elementCount a)) (error "Arrays do do have matching number of elements")) 
        (.getElements m data (int 0))
        (.getElements a data2 (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i) (aget data2 i))))) 
        (.setElements m data)))
    ([m f a more]
      (mp/assign! m (mp/element-map m f a more))))
  (element-reduce
    ([m f]
      (reduce f (mp/element-seq m)))
    ([m f init]
      (reduce f init (mp/element-seq m)))))

(def math-op-mapping
  '[(abs Ops/ABS)
	  (acos Ops/ACOS)
	  (asin Ops/ASIN)
	  (atan Ops/ATAN)
	  (cbrt Ops/CBRT)
	  (ceil Ops/CEIL)
	  (cos Ops/COS)
	  (cosh Ops/COSH)
	  (exp Ops/EXP)
	  (floor Ops/FLOOR)
	  (log Ops/LOG)
	  (log10 Ops/LOG10)
	  (round Ops/RINT)
	  (signum Ops/SIGNUM)
	  (sin Ops/SIN)
	  (sinh Ops/SINH)
	  (sqrt Ops/SQRT)
	  (tan Ops/TAN)
	  (tanh Ops/TANH)
   	(to-degrees Ops/TO_DEGREES)
	  (to-radians Ops/TO_RADIANS)])

(eval
`(extend-protocol mp/PMathsFunctions
   INDArray
    ~@(map 
       (fn [[fname op]] 
         `(~fname [~'m] (with-clone [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)
   AMatrix
    ~@(map 
       (fn [[fname op]] 
         `(~fname [~'m]
                  (with-clone [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)
   AVector
    ~@(map 
       (fn [[fname op]] 
         `(~fname [~'m]
                  (with-clone [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)))

(eval
`(extend-protocol mp/PMathsFunctionsMutable
   INDArray
    ~@(map 
       (fn [[fname op]] 
         (let [fname (symbol (str fname "!"))]
           `(~fname [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)
   AMatrix
    ~@(map 
       (fn [[fname op]] 
         (let [fname (symbol (str fname "!"))]
           `(~fname [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)
   AVector
    ~@(map 
       (fn [[fname op]] 
         (let [fname (symbol (str fname "!"))]
           `(~fname [~'m] (.applyTo ~op ~'m)))) 
       math-op-mapping)))

;; TODO printing
;; we want to print in a form that can be constructed again
;;(defmethod print-method AVector [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))
;;(defmethod print-method AScalar [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))
;;(defmethod print-method AMatrix [x writer] (.write writer (str "#" (.getCanonicalName (class x)) (str x))))

;; registration

(imp/register-implementation (Vector/of (double-array [0])))