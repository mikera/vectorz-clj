(ns mikera.vectorz.matrix-api
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz AVector Vectorz Vector AScalar Vector3 Ops])
  (:import [mikera.vectorz Scalar])
  (:import [mikera.vectorz.impl IndexVector])
  (:import [mikera.arrayz Arrayz INDArray Array])
  (:import [mikera.indexz AIndex Index])
  (:import [java.util List])
  (:import [mikera.transformz ATransform])
  (:import [mikera.matrixx.decompose QR IQRResult Cholesky ICholeskyResult ICholeskyLDUResult])
  (:import [mikera.matrixx.decompose SVD ISVDResult LUP ILUPResult Eigen IEigenResult])
  (:import [mikera.matrixx.solve Linear])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(declare vectorz-coerce* avector-coerce*)

;; =======================================================================
;; Macros and helper functions
;;
;; Intended to be internal to vectorz-clj implementation

(defmacro tag-symbol [tag form]
  (let [tagged-sym (vary-meta (gensym "res") assoc :tag tag)]
    `(let [~tagged-sym ~form] ~tagged-sym)))

(defn vectorz-type? [tag]
  (let [^String stag (if (class? tag) (.getName ^Class tag) (str tag))]
    (or (.startsWith stag "mikera.vectorz.")
        (.startsWith stag "mikera.matrixx.")
        (.startsWith stag "mikera.indexz.")
        (.startsWith stag "mikera.arrayz."))))

(defmacro vectorz?
  "Returns true if v is a vectorz class (i.e. an instance of mikera.arrayz.INDArray)"
  ([a]
    `(instance? INDArray ~a)))

(defmacro vectorz-coerce 
  "Coerces the argument to a vectorz INDArray. Broadcasts to the shape of an optional target if provided."
  ([x]
    (if (and (symbol? x) (vectorz-type? (:tag (meta x))))
      x ;; return tagged symbol unchanged
      `(tag-symbol mikera.arrayz.INDArray
               (let [x# ~x]
                 (if (instance? INDArray x#) x# (vectorz-coerce* x#))))))
  ([target x]
    `(let [m# ~target
           x# (vectorz-coerce ~x)]
       (tag-symbol mikera.arrayz.INDArray
         (if (< (.dimensionality x#) (.dimensionality m#)) 
           (.broadcastLike x# m#) 
           x#)))))

(defmacro vectorz-clone 
  "Coerces the argument to a new (cloned) vectorz INDArray"
  ([x]
  `(tag-symbol mikera.arrayz.INDArray
               (let [x# ~x]
                 (if (instance? INDArray x#) (.clone ^INDArray x#) (vectorz-coerce* x#))))))

(defmacro avector-coerce 
  "Coerces an argument x to an AVector instance, of the same size as m"
  ([m x]
    `(tag-symbol mikera.vectorz.AVector
                 (let [x# ~x] 
                   (if (instance? AVector x#) x# (avector-coerce* ~m x#)))))
  ([x]
    `(tag-symbol mikera.vectorz.AVector
                 (let [x# ~x] 
                   (if (instance? AVector x#) x# (avector-coerce* x#))))))

(defmacro amatrix-coerce
  "Coerces an argument x to an AMatrix instance"
  ([m x]
    `(tag-symbol mikera.matrixx.AMatrix
                 (let [x# ~x] 
                   (if (instance? AMatrix x#) x# (amatrix-coerce* ~m x#)))))
  ([x]
    `(tag-symbol mikera.matrixx.AMatrix
                 (let [x# ~x] 
                   (if (instance? AMatrix x#) x# (amatrix-coerce* x#)))))) 

(defmacro with-clone 
  "Executes the body with a cloned version of the specfied symbol/expression binding. Returns the cloned object."
  ([[sym exp] & body]
    (let []
      (when-not (symbol? sym) (error "Symbol required for with-clone binding"))
      `(let [~sym (.clone ~(if exp exp sym))]
         ~@body
         ~sym))))

(defmacro with-vectorz-clone 
  "Executes the body with a cloned version of the specfied symbol/expression binding. Returns the cloned object."
  ([[sym exp] & body]
    (let []
      (when-not (symbol? sym) (error "Symbol required for with-clone binding"))
      `(let [~sym (vectorz-clone ~(if exp exp sym))]
         ~@body
         ~sym))))

(defmacro with-broadcast-clone 
  "Executes body with a broadcasted clone of a and a broadcasted INDArray version of b. 
   Returns the broadcasted clone of a."
  ([[a b] & body]
     (when-not (and (symbol? a) (symbol? b)) (error "Symbols required for with-broadcast-clone binding"))
     (let []
      `(let [~b (vectorz-coerce ~a ~b)
             ~a (.broadcastCloneLike ~a ~b)]
         ~@body
         ~a))))

(defmacro with-broadcast-coerce 
  "Executes body with a and a coerced INDArray version of b. Returns result of body."
  ([[a b] & body]
     (when-not (and (symbol? a) (symbol? b)) (error "Symbols required for with-broadcast-clone binding"))
     (let []
      `(let [~b (vectorz-coerce ~a ~b)
             ~a (.broadcastLike ~a ~b)]
         ~@body))))

(def ^{:tag Class :const true} INT-ARRAY-CLASS (Class/forName "[I"))

(defmacro int-array-coerce
  "Coerces an arbitrary object to an int array"
  ([m]
    `(tag-symbol ~'ints          
       (let [m# ~m] 
         (cond
           (instance? INT-ARRAY-CLASS m#) m#
           (sequential? m#) (int-array m#)
           :else (int-array (mp/element-seq m#)))))))


(defmacro with-indexes 
  "Executes body after binding int indexes from the given indexing object"
  ([[syms ixs] & body]
	  (let [n (count syms)
	        isym (gensym)]
	    `(let [~isym ~ixs]
	       (cond
	         (instance? INT-ARRAY-CLASS ~isym)
             (let [~isym ~(vary-meta isym assoc :tag "[I")
	                 ~@(interleave 
	                     syms 
	                     (map (fn [i] `(int (aget ~isym ~i)) ) (range n)))] ~@body)
	         (instance? clojure.lang.IPersistentVector ~isym)
	           (let [~isym ~(vary-meta isym assoc :tag 'clojure.lang.IPersistentVector)
	                 ~@(interleave 
	                     syms 
	                     (map (fn [i] `(int (.nth ~isym ~i)) ) (range n)))] ~@body)
           :else
	           (let [[~@syms] (seq ~isym)] ~@body))))))

(defn avector-coerce* 
  "Coerces to an AVector instance, broadcasting if necessary" 
  (^AVector [^AVector v m]
	  (cond 
      (number? m) 
        (let [r (Vectorz/newVector (.length v))] (.fill r (double m)) r)
      :else (.broadcastLike ^INDArray (vectorz-coerce* m) v)))
  (^AVector [m]
    (cond
	    (instance? AVector m) m
      (== (mp/dimensionality m) 1)
        (let [len (ecount m)
              r (Vectorz/newVector len)]
          (assign! r m) r)
      :else (Vectorz/toVector ^INDArray (vectorz-coerce* m))))) 

(defn amatrix-coerce* 
  "Coerces to an AMatrix instance, broadcasting if necessary" 
  (^AMatrix [^AMatrix v m]
	  (cond
      (number? m) 
        (let [r (Matrix/create (.rowCount v) (.columnCount v))] (.fill r (double m)) r)
      :else (.broadcastLike ^INDArray (vectorz-coerce* m) v)))
  (^AMatrix [m]
    (cond
	    (instance? AMatrix m) m
      :else (Matrixx/toMatrix ^INDArray (vectorz-coerce* m))))) 

    
(defn vectorz-coerce* 
  "Function to attempt conversion to a Vectorz INDArray object. May return nil if conversion fails."
  (^INDArray [p]
	  (let [dims (dimensionality p)]
	    (cond
		    (instance? INDArray p) p
	      (== 0 dims)
	        (cond 
	          (number? p) (Scalar. (.doubleValue ^Number p))
	          (instance? AScalar p) p
            (nil? p) nil
	          :else (do
                   ;; (println (str "Coercing " p))
                   (Scalar. (double (mp/get-0d p)))))
		    (== 1 dims)
		      (try (Vector/wrap (mp/to-double-array p)) (catch Throwable e nil))
		    (== 2 dims)
		      (try (Matrixx/toMatrix (mp/convert-to-nested-vectors p)) (catch Throwable e nil))
		    :else 
	        (let [^List sv (mapv (fn [sl] (vectorz-coerce sl)) (slices p))]
	          (and (seq sv) (sv 0) (Arrayz/create sv)))))))

(defmacro double-coerce [x]
  `(let [x# ~x]
     (double (if (number? x#) x# (mp/get-0d x#)))))

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
                                 (Arrayz/create ^List (mapv (fn [_] (mp/new-matrix-nd m (next shape))) (range (first shape))))))
                (construct-matrix [m data]
                                  (cond 
                                    (instance? INDArray data)
                                      (.clone ^INDArray data)
                                    (mp/is-scalar? data) 
                                      (double-coerce data)
                                    (array? data) 
                                      (if (== 0 (mp/dimensionality data))
                                        (double-coerce data)
                                        (vectorz-coerce data))
                                    :default
                                      (let [vm (mp/construct-matrix [] data)] 
                                        ;; (println m vm (shape vm))
                                        (assign! (mp/new-matrix-nd m (shape vm)) vm)))))))
         ['mikera.vectorz.AVector 'mikera.matrixx.AMatrix 'mikera.vectorz.AScalar 'mikera.arrayz.INDArray 'mikera.indexz.AIndex]) ))

(defmacro with-keys
  [available required]
  (let [result-sym (gensym)]
           `(if ~required
              (let
                      [~result-sym {}
                       ~@(mapcat (fn [[k v]] `(~result-sym (if (some #{~k} ~required) 
                                                             (assoc ~result-sym ~k ~v)
                                                             ~result-sym)))
                                 available)]
                      ~result-sym)
              ~available)))

(extend-protocol mp/PQRDecomposition
  AMatrix
    (qr [m options]
      (let
        [result (cond 
                  (:compact options) (QR/decompose m true)
                  :else (QR/decompose m))]
        (with-keys {:Q (.getQ result) :R (.getR result)} (:return options)))))

(extend-protocol mp/PLUDecomposition
  AMatrix
    (lu [m options]
      (let
        [result (LUP/decompose m)]
        (with-keys {:L (.getL result) :U (.getU result) :P (.getP result)} (:return options)))))

(extend-protocol mp/PCholeskyDecomposition
  AMatrix
  (cholesky [m options] 
      (let
        [result (Cholesky/decompose m)]
        (if result
          (with-keys {:L (.getL result) :L* (.getU result)} (:return options))
          nil))))

(extend-protocol mp/PSVDDecomposition
  AMatrix
  (svd [m options] 
      (let
        [result (SVD/decompose m)]
        (if result
          (with-keys {:U (.getU result) :S (diagonal (.getS result)) :V* (.getTranspose (.getV result))} (:return options))
          nil))))

(extend-protocol mp/PNorm
  INDArray
    (norm [m p]
      (cond 
        (= java.lang.Double/POSITIVE_INFINITY p) (.elementMax m)
        (number? p) (Math/pow (.elementAbsPowSum m p) (/ 1 p))
        :else (error "p must be a number"))))

(extend-protocol mp/PMatrixRank
  AMatrix
    (rank [m]
      (let [{:keys [S]} (mp/svd m {:return [:S]})
            eps 1e-10]
        (reduce (fn [n x] (if (< (java.lang.Math/abs (double x)) eps) n (inc n))) 0 S))))

(extend-protocol mp/PSolveLinear
  AMatrix
    (solve [a b]
      (Linear/solve a (avector-coerce b))))

(extend-protocol mp/PLeastSquares
  AMatrix
    (least-squares [a b]
      (Linear/solveLeastSquares a (avector-coerce b))))

(extend-protocol mp/PTypeInfo
  INDArray
    (element-type [m] (Double/TYPE))
  AIndex
    (element-type [m] (Integer/TYPE)))

(extend-protocol mp/PMutableMatrixConstruction
  INDArray
    (mutable-matrix [m] (.clone m))
  AIndex
    (mutable-matrix [m] (.clone m))) 

(extend-protocol mp/PMatrixMutableScaling
  INDArray
    (scale! [m a]
      (.scale m (double-coerce a)))
    (pre-scale! [m a]
      (.scale m (double-coerce a)))
  AVector
    (scale! [m a]
      (.scale m (double-coerce a)))
    (pre-scale! [m a]
      (.scale m (double-coerce a))))

(extend-protocol mp/PNumerical
  INDArray
    (numerical? [m]
      true)
  AIndex
    (numerical? [m]
      true))

(extend-protocol mp/PSameShape
  INDArray 
    (same-shape? [a b]
      (if (instance? INDArray b)
        (.isSameShape a ^INDArray b)
        (clojure.core.matrix.utils/same-shape-object? (mp/get-shape a) (mp/get-shape b)))))

(extend-protocol mp/PDoubleArrayOutput
  INDArray
    (to-double-array [m] (.toDoubleArray m))
    (as-double-array [m] nil)
  Array
    (to-double-array [m] (.toDoubleArray m))
    (as-double-array [m] (.getArray m))
  AScalar
    (to-double-array [m] (let [arr (double-array 1)] (aset arr (int 0) (.get m)) arr))
    (as-double-array [m] nil)
  Vector
    (to-double-array [m] (.toDoubleArray m))
    (as-double-array [m] (.getArray m))
  AVector
    (to-double-array [m] (.toDoubleArray m))
    (as-double-array [m] nil)
  AMatrix
    (to-double-array [m] (.toDoubleArray (.asVector m)))
    (as-double-array [m] nil)
  Matrix
    (to-double-array [m] (.toDoubleArray (.asVector m)))
    (as-double-array [m] (.data m))) 

(extend-protocol mp/PVectorisable
  INDArray
    (to-vector [m]
      (.toVector m))
  AVector
    (to-vector [m]
      (.clone m)))

(extend-protocol mp/PMutableFill
  INDArray
  (fill!
    [m value]
    (.fill m (double-coerce value))))

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
          :else (error "Matrix does not have dimension: " x))))
  AIndex
    (dimensionality [m]
      1)
    (is-vector? [m]
      true)
    (is-scalar? [m]
      false)
    (get-shape [m]
      [(.length m)])
    (dimension-count [m x]
      (let [x (int x)]
        (cond 
          (== x 0) (.length m)
          :else (error "Index does not have dimension: " x)))))
    
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
      (with-indexes [[x] indexes]
        (.get m (int x))))
  AMatrix
    (get-1d [m x]
      (error "Can't access 1-dimensional index of a matrix"))
    (get-2d [m x y]
      (.get m (int x) (int y)))
    (get-nd [m indexes]
      (with-indexes [[x y] indexes]
        (.get m x y)))
  AIndex
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (error "Can't access 2-dimensional index of an Index"))
    (get-nd [m indexes]
      (with-indexes [[x] indexes]
        (.get m (int x)))))

(extend-protocol mp/PZeroDimensionConstruction
  INDArray
    (new-scalar-array 
      ([m]
        (Scalar/create 0.0))
      ([m value]
        (Scalar/create (double-coerce value)))))

(extend-protocol mp/PZeroDimensionAccess
  INDArray
    (get-0d [m]
      (.get m))
    (set-0d! [m value]
      (.set m (double-coerce value)))
  AScalar
    (get-0d [m]
      (.get m))
    (set-0d! [m value]
      (.set m (double-coerce value))))

(extend-protocol mp/PZeroDimensionSet
  INDArray
    (set-0d [m value] 
      (if (== 0 (.dimensionality m))
        (Scalar/create (double-coerce value))
        (error "Can't do 0-d set on " (class m))))
  AScalar
    (set-0d [m value] 
      (Scalar/create (double-coerce value))))

(extend-protocol mp/PImmutableMatrixConstruction
  INDArray
    (immutable-matrix [m]
      (.immutable m)))

(extend-protocol mp/PSpecialisedConstructors
  INDArray
    (identity-matrix [m dims] 
      (Matrixx/createIdentityMatrix (int dims)))
    (diagonal-matrix [m diagonal-values] 
      (Matrixx/createDiagonalMatrix (Vectorz/toVector diagonal-values))))

(extend-protocol mp/PPermutationMatrix
  INDArray
    (permutation-matrix [m permutation]
      (let [v (int-array-coerce permutation)]
        (mikera.matrixx.impl.PermutationMatrix/create v))))

(extend-protocol mp/PBroadcast
  INDArray 
    (broadcast [m target-shape]
      (.broadcast m (int-array-coerce target-shape)))) 

(extend-protocol mp/PBroadcastLike
  INDArray 
    (broadcast-like [m a]
      (vectorz-coerce m a))) 

(extend-protocol mp/PBroadcastCoerce
  INDArray
    (broadcast-coerce [m a]
      (vectorz-coerce m a)))

(extend-protocol mp/PReshaping
  INDArray 
    (reshape [m target-shape]
      (.reshape m (int-array target-shape)))
  Index 
    (reshape [m target-shape]
      (cond
        (== 1 (count target-shape))
          (Index/of (int-array (take (first target-shape) (seq m))))
        :else 
          (.reshape (IndexVector/of (.data m)) (int-array target-shape))))) 

(extend-protocol mp/PZeroCount
  INDArray
    (zero-count [m] (- (.elementCount m) (.nonZeroCount m))))


(extend-protocol mp/PArrayMetrics
  INDArray
    (nonzero-count [m] (.nonZeroCount m)))

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
    (set-1d [m row v] (error "Can't do 1-dimensional set on a 0-d array!"))
    (set-2d [m row column v] (error "Can't do 2-dimensional set on a 0-d array!"))
    (set-nd [m indexes v]
      (if (== 0 (count indexes))
        (Scalar/create (double v))
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
      (let [m (.clone m)] (.set m (int row) (int column) (double v)) m))
    (set-nd [m indexes v]
      (if (== 2 (count indexes))
        (with-clone [m] (.set m (int (first indexes)) (int (second indexes)) (double v)))
        (error "Can't do " (count indexes) "-dimensional set on a 2D matrix!")))
    (is-mutable? [m] (.isFullyMutable m)))
    
(extend-protocol mp/PIndexedSettingMutable
  INDArray
    (set-1d! [m row v] 
      (.set m (int row) (double v))) ;; double is OK: v should only be a java.lang.Number instance
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
  AIndex
    (set-1d! [m row v] (.set m (int row) (int v)))
    (set-2d! [m row column v] (error "Can't do 2-dimensional set on a 1D index!"))
    (set-nd! [m indexes v]
      (if (== 1 (count indexes))
        (.set m (int (first indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 1D index!"))) 
  AMatrix
    (set-1d! [m row v] (error "Can't do 1-dimensional set on a 2D matrix!"))
    (set-2d! [m row column v] (.set m (int row) (int column) (double v)))
    (set-nd! [m indexes v]
      (if (== 2 (count indexes))
        (.set m (int (first indexes)) (int (second indexes)) (double v))
        (error "Can't do " (count indexes) "-dimensional set on a 2D matrix!"))))

(extend-protocol mp/PSparseArray
  INDArray
    (is-sparse? [m]
      (.isSparse m)))

(extend-protocol mp/PMatrixEquality
  INDArray
    (matrix-equals [a b]
      (if (instance? INDArray b)
        (.equals a ^INDArray b)
        (.equals a (vectorz-coerce b)))))

(extend-protocol mp/PMatrixEqualityEpsilon
  INDArray 
    (matrix-equals-epsilon [a b eps]
      (if (instance? INDArray b)
        (.epsilonEquals a ^INDArray b (double eps))
        (.epsilonEquals a (vectorz-coerce b) (double eps)))))

(extend-protocol mp/PMatrixSlices
  INDArray
    (get-row [m i]
      (if (== 2 (mp/dimensionality m))
        (.slice m (int i))
        (error "Can't get row of array with dimensionality: " (mp/dimensionality m))))
    (get-column [m i]
      (if (== 2 (mp/dimensionality m))
        (.slice m (int 1) (int i))
        (error "Can't get column of array with dimensionality: " (mp/dimensionality m))))
    (get-major-slice [m i]
      (.sliceValue m (int i)))
    (get-slice [m dimension i]
      (let [dimension (int dimension)]
        (.slice m dimension (int i))))
  AVector
    (get-row [m i]
      (error "Can't access row of a 1D vector!"))
    (get-column [m i]
      (error "Can't access column of a 1D vector!"))
    (get-major-slice [m i]
      (.sliceValue m (int i)))
    (get-slice [m dimension i]
      (if (== 0 dimension)
        (.sliceValue m (int i))
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

(extend-protocol mp/PRowSetting
  AMatrix
    (set-row [m i row]
      (with-clone [m]
        (.setRow m (int i) (avector-coerce (.getRow m 0) row))))
    (set-row! [m i row]
      (.setRow m (int i) (avector-coerce (.getRow m 0) row))))

(extend-protocol mp/PColumnSetting
  AMatrix
    (set-column [m i v]
      (with-clone [m]
        (.setColumn m (int i) (avector-coerce (.getColumn m 0) v))))
    (set-column! [m i v]
      (.setColumn m (int i) (avector-coerce (.getColumn m 0) v))))

(extend-protocol mp/PSliceView
  INDArray
    (get-major-slice-view [m i] 
      (.slice m (int i))))

(extend-protocol mp/PSliceSeq
  INDArray  
    (get-major-slice-seq [m] 
      (seq (.getSlices m)))
  AVector  
    (get-major-slice-seq [m] 
      (seq (or (.asDoubleArray m) (.toDoubleArray m))))
  Index
    (get-major-slice-seq [m] 
      (seq (.getData m))))

(extend-protocol mp/PSliceViewSeq
  INDArray
    (get-major-slice-view-seq [m] 
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
           (.fill m (double-coerce source))
        :else 
           (.set m (vectorz-coerce source))))
    (assign-array! 
      ([m arr] (dotimes [i (count arr)] (.set m (int i) (double (nth arr i)))))
      ([m arr start length] 
        (let [length (long length) start (long start)] 
          (dotimes [i length] (.set m (int i) (double (nth arr (+ i start))))))))
    
  INDArray
    (assign! [m source] 
      (.set m (vectorz-coerce m source)))
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
  INDArray
    (subvector [m start length]
      (let [dims (.dimensionality m)]
        (if (== 1 dims)
          (.subVector (.asVector m) (int start) (int length))
          (error "Can't take subvector of " dims "-D array"))))
  
  AVector
    (subvector [m start length]
      (.subVector m (int start) (int length)))) 

(extend-protocol mp/PSubMatrix
  AMatrix
    (submatrix [m index-ranges]
      (let [[rr cr] index-ranges
            s1 (int (if rr (first rr) 0))
            s2 (int (if cr (first cr) 0))
            l1 (int (if rr (second rr) (.rowCount m)))
            l2 (int (if cr (second cr) (.columnCount m)))]
        (.subMatrix m s1 l1 s2 l2)))
  AVector
    (submatrix [m index-ranges]
      (let [[rr] index-ranges
            s1 (int (if rr (first rr) 0))
            l1 (int (if rr (second rr) (.length m)))]
        (.subVector m s1 l1)))) 

;; protocols for indexed access

(extend-protocol mp/PSelect
  AVector
    (select [a args] 
      (let [ixs (int-array-coerce (first args))]
        (.select a ixs))))

(extend-protocol mp/PSetSelection
  AVector
    (set-selection [a args values] 
      (let [ixs (int-array-coerce (first args))
            sv (.select a ixs)
            vs (avector-coerce sv values)]
        (.set sv vs))))

(extend-protocol mp/PIndicesAccess
  INDArray
  (get-indices [a indices] 
    (let [c (int (count indices))
          r (Vectorz/newVector c)]
      (doseq-indexed [ix indices i]
        (.unsafeSet r (int i) (.get a (int-array-coerce ix))))
      r)))

(extend-protocol mp/PIndicesSetting
  INDArray
    (set-indices [a indices values] 
      (mp/set-indices! (.clone a) indices values))
    (set-indices! [a indices values] 
      (let [c (int (count indices))
            vs (avector-coerce values)]
        (doseq-indexed [ix indices i]
          (.set a (int-array-coerce ix) (.get vs (int i)))))))

;; protocols for elementwise ops

(extend-protocol mp/PSummable
  INDArray 
    (element-sum [m]
      (.elementSum m))
  AVector
    (element-sum [m]
      (.elementSum m))
  AMatrix
    (element-sum [m]
      (.elementSum m))
  AScalar
    (element-sum [m]
      (.get m)))

(extend-protocol mp/PMatrixAdd
  mikera.vectorz.AScalar
    (matrix-add [m a]
      (with-broadcast-coerce [m a] (.addCopy m a)))
    (matrix-sub [m a]
      (with-broadcast-coerce [m a] (.subCopy m a)))
  mikera.vectorz.AVector
    (matrix-add [m a]
      (with-broadcast-coerce [m a] (.addCopy m a)))
    (matrix-sub [m a]
      (with-broadcast-coerce [m a] (.subCopy m a)))
  mikera.matrixx.AMatrix
    (matrix-add [m a]
      (with-broadcast-coerce [m a] (.addCopy m a)))
    (matrix-sub [m a]
      (with-broadcast-coerce [m a] (.subCopy m a)))
  INDArray
    (matrix-add [m a]
      (with-broadcast-coerce [m a] (.addCopy m a)))
    (matrix-sub [m a]
      (with-broadcast-coerce [m a] (.subCopy m a))))

(extend-protocol mp/PMatrixAddMutable
  INDArray
    (matrix-add! [m a]
      (.add m (vectorz-coerce a)))
    (matrix-sub! [m a]
      (.sub m (vectorz-coerce a)))
  AScalar
    (matrix-add! [m a]
      (.add m (double-coerce a)))
    (matrix-sub! [m a]
      (.sub m (double-coerce a)))
  AVector
    (matrix-add! [m a]
      (.add m (avector-coerce m a)))
    (matrix-sub! [m a]
      (.sub m (avector-coerce m a)))
  AMatrix
    (matrix-add! [m a]
      (.add m (amatrix-coerce m a)))
    (matrix-sub! [m a]
      (.sub m (amatrix-coerce m a))))

(extend-protocol mp/PVectorOps
  INDArray
    (vector-dot [a b]
      (.dotProduct (avector-coerce a) (avector-coerce a b)))
    (length [a]
      (.magnitude (avector-coerce a)))
    (length-squared [a]
      (.magnitudeSquared (avector-coerce a)))
    (normalise [a]
      (with-clone [a] (.normalise (avector-coerce a))))
  
  AVector
    (vector-dot [a b]
      (.dotProduct a (avector-coerce a b)))
    (length [a]
      (.magnitude a))
    (length-squared [a]
      (.magnitudeSquared a))
    (normalise [a]
      (with-clone [a] (.normalise a))))

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

(extend-protocol mp/PTransposeInPlace
  AMatrix (transpose! [m] (.transposeInPlace m))
  AVector (transpose! [m] m)
  AScalar (transpose! [m] m)) 

(extend-protocol mp/PVectorCross
  INDArray
    (cross-product [a b]
      (let [v (Vector3. (avector-coerce a))]
        (.crossProduct v (avector-coerce a b))
        v))
    (cross-product! [a b]
      (assign! a (mp/cross-product a b)))
  AVector
    (cross-product [a b]
      (let [v (Vector3. a)]
        (.crossProduct v (avector-coerce a b))
        v))
    (cross-product! [a b]
      (.crossProduct a (avector-coerce a b)))) 

(extend-protocol mp/PMatrixCloning
  INDArray (clone [m] (.clone m))
  AScalar (clone [m] (.clone m))
  AVector (clone [m] (.clone m))
  AMatrix	(clone [m] (.clone m))
  AIndex	(clone [m] (.clone m)))

(extend-protocol mp/PCoercion
  INDArray
    (coerce-param [m param]
      (if (number? param)
        param
        (vectorz-coerce param)))
  AIndex
    (coerce-param [m param]
      (if (== 1 (mp/dimensionality param))
        (Index/of (int-array (mp/element-seq param))) 
        (error "Cannot coerce to Index with shape: " (vec (mp/get-shape param))))))

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
        (mapv mp/convert-to-nested-vectors (.getSlices m))))
  AIndex
    (convert-to-nested-vectors [m]
      (vec m))
  Index
    (convert-to-nested-vectors [m]
      (vec (.getData m))))

(extend-protocol mp/PMatrixDivide
  INDArray
  (element-divide 
    ([m] 
      (with-clone [m] (.reciprocal m)))
    ([m a] 
      (with-broadcast-clone [m a] (.divide m a)))))

(extend-protocol mp/PMatrixMultiply
  AScalar
    (matrix-multiply [m a]
      (with-vectorz-clone [a] (.multiply a (.get m))))
    (element-multiply [m a]
      (with-vectorz-clone [a] (.multiply a (.get m))))
  AVector
    (matrix-multiply [m a]
      (.innerProduct m (vectorz-coerce a)))
    (element-multiply [m a]
      (with-broadcast-coerce [m a] (.multiplyCopy m a)))
  AMatrix
    (matrix-multiply [m a]
      (cond 
        (instance? AVector a) 
          (let [^AVector r (Vectorz/newVector (.rowCount m))] 
            (.transform m ^AVector a r)
            r)
        (instance? AMatrix a) (.innerProduct m ^AMatrix a) 
        (number? a) (with-clone [m] (.multiply m (double a)))
        :else (.innerProduct m (vectorz-coerce a))))
    (element-multiply [m a]
      (with-broadcast-clone [m a] (.multiply m a)))
  INDArray
    (matrix-multiply [m a]
      (if-let [^INDArray a (vectorz-coerce a)]
        (.innerProduct m a)
        (error "Can't convert to vectorz representation: " a)))
    (element-multiply [m a]
      (with-broadcast-clone [m a] (.multiply m a))))

(extend-protocol mp/PMatrixMultiplyMutable
  INDArray
    (matrix-multiply! [m a]
      (mp/assign! m (mp/inner-product m a)))
    (element-multiply! [m a]
      (.multiply m (vectorz-coerce a))))

(extend-protocol mp/PMatrixDivideMutable
  INDArray
    (element-divide!
      ([m] (.reciprocal m))
      ([m a] (.divide m (vectorz-coerce a)))))

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
  INDArray
    (add-product! [m a b]
      (.add m (vectorz-coerce (mp/element-multiply a b))))
  AVector
    (add-product! [m a b]
      (.addProduct m (avector-coerce m a) (avector-coerce m b)))) 

(extend-protocol mp/PAddScaled
  AVector
    (add-scaled [m a factor]
      (with-clone [m] 
        (.addMultiple m (avector-coerce m a) (double-coerce factor))))) 

(extend-protocol mp/PAddScaledMutable
  AMatrix
    (add-scaled! [m a factor]
      (.addMultiple m (vectorz-coerce a) (double-coerce factor)))
  AVector
    (add-scaled! [m a factor]
      (.addMultiple m (avector-coerce m a) (double-coerce factor)))) 

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
    (scale [m a] (vectorz-scale m (double-coerce a)))
    (pre-scale [m a] (vectorz-scale m (double-coerce a)))
  AVector
    (scale [m a] (vectorz-scale m (double-coerce a)))
    (pre-scale [m a] (vectorz-scale m (double-coerce a)))
  AMatrix
    (scale [m a] (vectorz-scale m (double-coerce a)))
    (pre-scale [m a] (vectorz-scale m (double-coerce a)))
  INDArray
    (scale [m a] (vectorz-scale m (double-coerce a)))
    (pre-scale [m a] (vectorz-scale m (double-coerce a))))

(extend-protocol mp/PVectorTransform
  ATransform
    (vector-transform [m v] 
      (if (instance? AVector v) 
        (.transform m ^AVector v)
        (.transform m (avector-coerce v))))
    (vector-transform! [m v] 
      (if (instance? AVector v) 
        (.transformInPlace m ^AVector v)
        (assign! v (transform m v)))))

(extend-protocol mp/PMutableVectorOps
  INDArray
    (normalise! [a]
      (if (== 1 (.dimensionality a))
        (.normalise (.asVector a))
        (error "Can't normalise something that isn't a 1D vector!")))
  AVector
    (normalise! [a]
      (.normalise a)))

(extend-protocol mp/PMatrixOps
  INDArray
    (trace [m]
      (.trace (amatrix-coerce m)))
    (determinant [m]
      (.determinant (amatrix-coerce m)))
    (inverse [m]
      (.inverse (amatrix-coerce m)))
  AMatrix
    (trace [m]
      (.trace m))
    (determinant [m]
      (.determinant m))
    (inverse [m]
      (.inverse m)))

(extend-protocol mp/PMatrixPredicates
  INDArray
    (identity-matrix?
      [m]
      (and 
        (== 2 (.dimensionality m))
        (identity-matrix? (Matrixx/toMatrix m)))) ;; TODO: make cheaper
    (zero-matrix?
      [m]
      (.isZero m))
    (symmetric?
      [m]
      (case (.dimensionality m) ; should be 1, 3, 4, ...; never 2
        1 true
        (equals m (transpose m))))
  AMatrix
    (identity-matrix?
      [m]
      (and
        (.isSquare m)    ;; workaround for vectorz bug until 0.21.1
        (.isIdentity m)))
    (zero-matrix?
      [m]
      (.isZero m))
    (symmetric?
      [m]
      (.isSymmetric m)))

(extend-protocol mp/PSquare
  INDArray
    (square [m]
      (with-clone [m] (.square m)))
  AVector
    (square [m]
      (with-clone [m] (.square m))))

(extend-protocol mp/PExponent
  INDArray
    (element-pow [m exponent]
      (if (number? m)
        (with-clone [m] (.pow m (double-coerce exponent)))
        (mp/element-map m #(Math/pow %1 %2) (vectorz-coerce m exponent)))))

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
      1)
  AIndex
    (element-count [m]
      (.length m)))

(extend-protocol mp/PSparse
  INDArray
    (sparse-coerce [m data]
      (cond 
        (== 0 (mp/dimensionality data)) (Scalar. (double-coerce data))
        (vectorz? data) (.sparse ^INDArray data)
        :else (let [ss (map (fn [s] (.sparse (vectorz-coerce s))) (mp/get-major-slice-seq data))]
               (.sparse (Arrayz/create (object-array ss))))))
    (sparse [m]
      (.sparse m)))

(extend-protocol mp/PSliceJoin
  INDArray
    (join [m a]
      ;; TODO: wait for better join implementation in Vectorz for INDArray
      (Arrayz/create ^List (vec (concat (slices m) (slices (vectorz-coerce a))))))
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

(extend-protocol mp/PVectorDistance
  AVector
    (distance [a b]
      (.distance a (avector-coerce b))))

(extend-protocol mp/PElementMinMax
  INDArray
  (element-min [m]
    (.elementMin m))
  (element-max [m]
    (.elementMax m)))

(extend-protocol mp/PComputeMatrix
  INDArray
    (compute-matrix [m shape f]
      (let [dims (long (count shape))]
        (cond 
          (== 0 dims) (double (f))
          (== 1 dims) 
            (let [n (int (first shape))
                  v (Vector/createLength n)] 
              (dotimes [i n] (.set v (int i) (double (f i))))
              v)
          (== 2 dims)
            (let [n (int (first shape))
                  m (int (second shape))
                  v (Matrix/create n m)] 
              (dotimes [i n] 
                (dotimes [j m]
                  (.set v (int i) (int j) (double (f i j)))))
              v)
          :else 
            (Arrayz/create 
              (let [ns (next shape)]
                (mapv #(mp/compute-matrix m ns (fn [& ixs] (apply f % ixs))) (range (first shape)))))))))

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
      (reduce f init (mp/element-seq m))))
  
  AVector
  (element-seq
    [m]
    (let [ec (.length m)
          ^doubles data (or (.asDoubleArray m)
                          (let [arr (double-array ec)]
                            (.getElements m arr (int 0))
                            arr))]
      (seq data)))
  (element-map
    ([m f]
      (let [ec (.elementCount m)
            ^doubles data (double-array ec)]
        (dotimes [i ec] (aset data i (double (f (.unsafeGet m i))))) 
        (Vector/wrap data)))
    ([m f a]
      (let [ec (.elementCount m)
            a (avector-coerce m a) 
            ^doubles data (double-array ec)]
        (dotimes [i ec] (aset data i (double (f (.unsafeGet m i) (.unsafeGet a i))))) 
        (Vector/wrap data)))
    ([m f a more]
      (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f a more))))
  (element-map!
    ([m f]
      (let [ec (.elementCount m)]
        (dotimes [i ec] (.unsafeSet m i (double (f (.unsafeGet m i))))) ))
    ([m f a]
      (let [ec (.elementCount m)
            a (avector-coerce m a)]
        (dotimes [i ec] (.unsafeSet m i (double (f (.unsafeGet m i) (.unsafeGet a i))))) ))
    ([m f a more]
      (mp/assign! m (mp/element-map m f a more))))
  (element-reduce
    ([m f]
      (let [n (.length m)] 
        (cond 
          (== 0 n) (f) 
          (== 1 n) (.unsafeGet m 0) 
          :else (loop [v ^Object (.unsafeGet m 0) i 1]
                  (if (< i n)
                    (recur (f v (.unsafeGet m i)) (inc i))
                    v)))))
    ([m f init]
      (let [n (.length m)] 
        (cond 
          (== 0 n) init 
          :else (loop [v init i 0]
                  (if (< i n)
                    (recur (f v (.unsafeGet m i)) (inc i))
                    v))))))
  
  AMatrix
  (element-seq
    [m]
    (let [ec (.elementCount m)
          ^doubles data (or (.asDoubleArray m)
                          (let [arr (double-array ec)]
                            (.getElements m arr (int 0))
                            arr))]
      (seq data)))
  (element-map
    ([m f]
      (let [rc (.rowCount m)
            cc (.columnCount m)
            ec (* rc cc)
            ^doubles data (double-array ec)]
        (.getElements m data (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i))))) 
        (Matrix/wrap rc cc data)))
    ([m f a]
      (let [a (amatrix-coerce m a)
            rc (.rowCount m)
            cc (.columnCount m)
            ec (* rc cc)
            ^doubles data (double-array ec)
            ^doubles data2 (double-array ec)]
        (.getElements m data (int 0))
        (.getElements a data2 (int 0))
        (dotimes [i ec] (aset data i (double (f (aget data i) (aget data2 i))))) 
        (Matrix/wrap rc cc data)))
    ([m f a more]
      (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f a more))))
  (element-map!
    ([m f]
      (let [rc (.rowCount m)
            cc (.columnCount m)]
        (dotimes [i rc] 
          (dotimes [j cc] 
            (.unsafeSet m i j (double (f (.unsafeGet m i j)))))) ))
    ([m f a]
      (let [a (amatrix-coerce m a)
            rc (.rowCount m)
            cc (.columnCount m)]
        (dotimes [i rc] 
          (dotimes [j cc] 
            (.unsafeSet m i j (double (f (.unsafeGet m i j) (.unsafeGet a i j)))))) ))
    ([m f a more]
      (mp/assign! m (mp/element-map m f a more))))
  (element-reduce
    ([m f]
      (mp/element-reduce (.asVector m) f))
    ([m f init]
      (mp/element-reduce (.asVector m) f init)))
  
  
 AIndex
  (element-seq
    [m]
    (seq m))
  (element-map
    ([m f]
      (let [ec (.length m)
            ^ints data (int-array ec)]
        (dotimes [i ec] (aset data i (int (f (.get m i))))) 
        (Index/of data)))
    ([m f a]
      (let [ec (.length m)
            ^ints data (int-array ec)]
        (dotimes [i ec] (aset data i (int (f (.get m i) (mp/get-1d a i))))) 
        (Index/of data)))
    ([m f a more]
      (mp/element-map (mp/convert-to-nested-vectors m) f a more)))
  (element-map!
    ([m f]
      (let [ec (.length m)]
        (dotimes [i ec] (.set m i (int (f (.get m i))))) ))
    ([m f a]
      (let [ec (.length m)]
        (dotimes [i ec] (.set m i (int (f (.get m i) (mp/get-1d a i))))) ))
    ([m f a more]
      (mp/assign! m (mp/element-map m f a more))))
  (element-reduce
    ([m f]
      (let [n (.length m)] 
        (cond 
          (== 0 n) (f) 
          (== 1 n) (.get m 0) 
          :else (loop [v ^Object (.get m 0) i 1]
                  (if (< i n)
                    (recur (f v (.get m i)) (inc i))
                    v)))))
    ([m f init]
      (let [n (.length m)] 
        (cond 
          (== 0 n) init 
          :else (loop [v init i 0]
                  (if (< i n)
                    (recur (f v (.get m i)) (inc i))
                    v)))))))

;; ==============================================================
;; Generator for mathematical functions

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
       math-op-mapping)
   AScalar
    ~@(map 
       (fn [[fname op]] 
         `(~fname [~'m]
                  (.apply ~op (.get ~'m)))) 
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

:OK
