(ns mikera.vectorz.matrix-api
  (:use core.matrix)
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.matrixx AMatrix Matrixx MatrixMN])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(extend-protocol PIndexedAccess
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
          (get-2d m x y)))))


(extend-protocol PMatrixSlices
  mikera.vectorz.AVector
    (get-row [m i]
      (.get m (int i)))
    (get-column [m i]
      (error "Can't access column of a vector!"))
  mikera.matrixx.AMatrix
    (get-row [m i]
      (.getRow m (int i)))
    (get-column [m i]
      (.getColumn m (int i))))

(extend-protocol PMatrixAdd
  mikera.vectorz.AVector
    (matrix-add [m a]
      (v/add m (coerce m a)))
    (matrix-sub [m a]
      (v/sub m (coerce m a)))
  mikera.matrixx.AMatrix
    (matrix-add [m a]
      (let [m (m/clone m)] 
        (.add m (coerce m a))
        m))
    (matrix-sub [m a]
      (let [m (m/clone m)] 
        (.addMultiple m (coerce m a) -1.0)
        m)))

(extend-protocol PVectorOps
  mikera.vectorz.AVector
    (vector-dot [a b]
      (.dotProduct a (coerce a b)))
    (length-squared [a]
      (.lengthSquared a))
    (normalise [a]
      (v/normalise a)))
    
(extend-protocol PCoercion
  mikera.vectorz.AVector
    (coerce-param [m param]
      (Vectorz/toVector param))
  mikera.matrixx.AMatrix
    (coerce-param [m param]
      (Matrixx/toMatrix param)))

(extend-protocol PMatrixMultiply
  mikera.vectorz.AVector
    (matrix-multiply [m a]
      (matrix-multiply (mikera.matrixx.impl.ColumnMatrix/wrap m) a))
    (scale [m a]
      (v/scale m a))
  mikera.matrixx.AMatrix
    (matrix-multiply [m a]
      (if (instance? mikera.vectorz.AVector a)
        (.transform m ^AVector a)
        (m/* m (coerce m a))))
    (scale [m a]
      (m/scale m a)))

(extend-protocol PMatrixDimensionInfo
  mikera.vectorz.AVector
    (dimensionality [m]
      1)
    (row-count [m]
      (.length m))
    (is-vector? [m]
      true)
    (column-count [m]
      1)
    (dimension-count [m x]
      (if (== x 0)
        (.length m)
        (error "Vector does not have dimension: " x)))
  mikera.matrixx.AMatrix
    (dimensionality [m]
      1)
    (row-count [m]
      (.rowCount m))
    (is-vector? [m]
      true)
    (column-count [m]
      (.columnCount m))
    (dimension-count [m x]
      (cond 
        (== x 0) (.rowCount m)
        (== x 1) (.columnCount m)
        :else (error "Matrix does not have dimension: " x))))
    
