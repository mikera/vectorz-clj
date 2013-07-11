(ns mikera.vectorz.core
  (:import [mikera.vectorz AVector Vectorz Vector Vector1 Vector2 Vector3 Vector4])
  (:import [mikera.arrayz INDArray])
  (:import [mikera.transformz Transformz])
  (:require [mikera.vectorz.matrix-api]) 
  (:require [mikera.cljutils.error :refer [error]]) 
  (:refer-clojure :exclude [+ - * / vec vec? vector subvec get set to-array empty]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ==================================================
;; Protocols

(defprotocol PVectorisable
  (to-vector [a]))

(extend-protocol PVectorisable
  (Class/forName "[D")
    (to-vector [coll]
      (Vectorz/create ^doubles coll))
  java.util.List
    (to-vector [coll]
       (Vectorz/create ^java.util.List coll))
  java.lang.Iterable 
    (to-vector [coll] 
       (Vectorz/create ^java.lang.Iterable coll))
  mikera.vectorz.AVector
    (to-vector [coll]
       (.clone coll)))

;; ==================================================
;; basic functions

(defn clone
  "Creates a (mutable) clone of a vector. May not be exactly the same class as the original vector."
  (^AVector [^AVector v]
    (.clone v)))

(defn ecount
  "Returns the number of elements in a vector"
  (^long [^AVector v]
    (.length v)))

(defn vec?
  "Returns true if v is a vector (i.e. an instance of mikera.vectorz.AVector)"
  ([v]
    (instance? AVector v)))

(defn vectorz?
  "Returns true if v is a vectorz class (i.e. an instance of mikera.arrayz.INDArray)"
  ([a]
    (instance? INDArray a)))

(defn get
  "Returns the component of a vector at a specific index position"
  (^double [^AVector v ^long index]
    (.get v (int index))))

(defn set
  "Sets the component of a vector at position i (mutates in place)"
  ([^AVector v ^long index ^double value]
    (.set v (int index) value)
    v))

(defn update
  "Returns a new vector with the component at position i updated"
  ([^AVector v ^long index ^double value]
    (mikera.vectorz.core/set (clone v) index value)))

;; =====================================================
;; vector predicates

(defn normalised?
  "Returns true if a vector is normalised (has unit length)"
  [^AVector v]
  (.isUnitLengthVector v))

;; ====================================================
;; vector constructors

(defn of 
  "Creates a vector from its numerical components"
  ([& xs]
    (let [len (int (count xs))
          ss (seq xs)
          ^AVector v (Vectorz/newVector len)]
       (loop [i (int 0) ss ss]
         (if ss
           (do
             (.set v i (double (first ss)))
             (recur (inc i) (next ss)))
           v)))))

(defn vec
  "Creates a vector from a collection, a sequence or anything else that implements the PVectorisable protocol"
  ([coll]
  (cond 
    (vec? coll) (clone coll)
    (satisfies? PVectorisable coll) (to-vector coll)
    (sequential? coll) (apply of coll)
    :else (error "Can't create vector from: " (class coll)))))

(defn vec1
  "Creates a Vector1 instance"
  (^Vector1 []
    (Vector1.)) 
  (^Vector1 [coll]
    (if (number? coll) 
      (Vector1/of (double coll))
      (let [v (vec coll)]
        (if (instance? Vector1 v) 
          v
          (error "Can't create Vector1 from: " (str coll))))))) 

(defn vec2
  "Creates a Vector2 instance"
  (^Vector2 []
    (Vector2.)) 
  (^Vector2 [coll]
    (let [v (vec coll)]
      (if (instance? Vector2 v) 
        v
        (error "Can't create Vector2 from: " (str coll)))))
  (^Vector2 [x y]
    (Vector2/of (double x) (double y)))) 

(defn vec3
  "Creates a Vector3 instance"
  (^Vector3 []
    (Vector3.)) 
  (^Vector3 [coll]
    (let [v (vec coll)]
      (if (instance? Vector3 v) 
        v
        (error "Can't create Vector3 from: " (str coll)))))
  (^Vector3 [x y z]
    (Vector3/of (double x) (double y) (double z)))) 

(defn vec4
  "Creates a Vector4 instance"
  (^Vector4 []
    (Vector4.)) 
  (^Vector4 [coll]
    (let [v (vec coll)]
      (if (instance? Vector4 v) 
        v
        (error "Can't create Vector4 from: " (str coll)))))
  (^Vector4 [x y z t]
    (Vector4/of (double x) (double y) (double z) (double t)))) 

(defn vector 
  "Creates a vector from zero or more numerical components."
  (^AVector [& xs]
    (vec xs)))


(defn create-length
  "Creates a vector of a specified length. Will use optimised primitive vectors for small lengths"
  (^AVector [len]
    (Vectorz/newVector (int len))))

(defn empty
  "Creates an empty vector of a specified length. Will use optimised primitive vectors for small lengths"
  (^AVector [len]
    (Vectorz/newVector (int len))))

(defn subvec
  "Returns a subvector of a vector. The subvector is a reference (i.e can be sed to modify the original vector)" 
  (^AVector [^AVector v start end]
    (.subVector v (int start) (int end))))

(defn join 
  "Joins two vectors together. The returned vector is a new reference vector that refers to the originals."
  (^AVector [^AVector a ^AVector b]
    (.join a b)))

;; ======================================
;; Conversions


(defn to-array 
  "Converts a vector to a double array"
  ([^AVector a]
    (.toArray a)))

(defn to-list 
  "Converts a vector to a list of doubles"
  ([^AVector a]
    (.toList a)))

;; =====================================
;; In-place operations

(defn assign! 
  "Fills a vector in place with the value of another vector" 
  ([^AVector a ^AVector new-value]
    (.set a new-value)
    a))

(defn add!
  "Add a vector to another (in-place)"
  (^AVector [^AVector dest ^AVector source]
    (.add dest source)
    dest))

(defn add-multiple!
  "Add a vector to another (in-place)"
  ([^AVector dest ^AVector source ^double factor]
    (.addMultiple dest source factor)
    dest))

(defn sub!
  "Subtract a vector from another (in-place)"
  (^AVector [^AVector dest ^AVector source]
    (.sub dest source)
    dest))

(defn mul!
  "Multiply a vector with another vector or scalar (in-place)"
  (^AVector [^AVector dest source]
    (if (number? source) 
      (.multiply dest (double source))
      (.multiply dest ^AVector source))
    dest))

(defn div!
  "Divide a vector by another vector or scalar (in-place)"
  (^AVector [^AVector dest source]
    (if (number? source) 
      (.divide dest (double source))
      (.divide dest ^AVector source))
    dest))

(defn normalise! 
  "Normalises a vector in place to unit length and returns it"
  ([^AVector a]
    (.normalise a)
    a))

(defn normalise-get-magnitude! 
  "Normalises a vector in place to unit length and returns its magnitude"
  (^double [^AVector a]
    (.normalise a)))

(defn negate! 
  "Negates a vector in place and returns it" 
  ([^AVector a]
    (.negate a)
    a))

(defn abs! 
  "Computes the absolute value of a vector in place and returns it" 
  ([^AVector a]
    (.abs a)
    a))

(defn scale! 
  "Scales a vector in place by a scalar numerical factor" 
  ([^AVector a ^double factor]
    (.scale a factor)
    a))

(defn scale-add! 
  "Scales a fector in place by a scalar numerical factor and adds a second vector" 
  ([^AVector a ^double factor ^AVector b]
    (.scaleAdd a factor b)
    a))

(defn add-weighted!
  "Create a weighted average of a vector with another in place. Numerical weight specifies the proportion of the second vector to use"
  (^AVector [^AVector dest ^AVector source weight]
    (.addWeighted dest source (double weight))
    dest))

(defn fill! 
  "Fills a vector in place with a specific numerical value" 
  ([^AVector a ^double value]
    (.fill a value)
    a))



;; =====================================
;; Special 3D functions

(defn cross-product!
  "Calculates the cross product of a 3D vector in place "
  (^Vector3 [^Vector3 a ^AVector b]
    (.crossProduct a b)
    a)) 

;; =====================================
;; Pure functional operations

(defn add
  "Add a vector to another"
  (^AVector [^AVector dest ^AVector source]
    (add! (clone dest) source)))

(defn add-multiple
  "Add a vector to another"
  (^AVector [^AVector dest ^AVector source ^double factor]
    (add-multiple! (clone dest) source factor)))

(defn sub
  "Subtract a vector from another"
  (^AVector [^AVector dest ^AVector source]
    (sub! (clone dest) source)))

(defn mul
  "Multiply a vector with another vector or scalar"
  (^AVector [^AVector dest source]
    (mul! (clone dest) source)))

(defn div
  "Divide a vector by another vector or scalar"
  (^AVector [^AVector dest source]
    (div! (clone dest) source)))

(defn interpolate
  (^AVector [^AVector a ^AVector b position]
    (let [^AVector result (clone a)]
      (.interpolate result b (double position))
      result))) 

(defn normalise
  "Normalises a vector to unit length and returns it"
  (^AVector [^AVector a]
    (normalise! (clone a))))

(defn negate 
  "Negates a vector and returns it" 
  (^AVector [^AVector a]
    (negate! (clone a))))

(defn abs 
  "Computes the absolute value of a vector and returns it" 
  (^AVector [^AVector a]
    (abs! (clone a))))

(defn scale 
  "Scales a vector by a scalar numerical factor" 
  ([^AVector a ^double factor]
    (scale! (clone a) factor)))

(defn scale-add 
  "Scales a fector by a scalar numerical factor and adds a second vector" 
  ([^AVector a factor ^AVector b]
    (scale-add! (clone a) factor b)))

(defn fill 
  "Fills a vector with a specific numerical value" 
  ([^AVector a ^double value]
    (fill! (clone a) value)))


(defn add-weighted
  "Create a weighted average of a vector with another. Numerical weight specifies the proportion of the second vector to use"
  (^AVector [^AVector dest ^AVector source weight]
    (add-weighted! (clone dest) source (double weight))))


;; =====================================
;; Arithmetic functions and operators

(defn approx=
  "Returns a boolean indicating whether the two vectors are approximately equal, +/- an optional tolerance" 
  ([^AVector a ^AVector b]
    (.epsilonEquals a b))
  ([^AVector a ^AVector b epsilon]
    (.epsilonEquals a b (double epsilon))))

(defn dot
  "Compute the dot product of two vectors"
  (^double [^AVector a ^AVector b]
    (.dotProduct a b)))

(defn magnitude
  "Return the magnitude of a vector (geometric length)" 
  (^double [^AVector a]
    (.magnitude a)))

(defn magnitude-squared
  "Return the squared magnitude of a vector. Slightly more efficient than getting the magnitude directly." 
  (^double [^AVector a]
    (.magnitudeSquared a)))

(defn distance 
  "Return the euclidean distance between two vectors" 
  (^double [^AVector a ^AVector b]
    (.distance a b)))

(defn distance-squared 
  "Return the squared euclidean distance between two vectors" 
  (^double [^AVector a ^AVector b]
    (.distanceSquared a b)))

(defn angle 
  "Return the angle between two vectors" 
  (^double [^AVector a ^AVector b]
    (.angle a b)))

(defn + 
  "Add one or more vectors, returning a new vector as the result"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [r (clone a)]
      (.add r b)
      r))
  (^AVector [^AVector a ^AVector b & vs] 
    (let [r (clone a)]
      (.add r b)
      (doseq [^AVector v vs]
        (.add r v))
      r)))

(defn - 
  "Substract one or more vectors"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [r (clone a)]
      (.sub r b)
      r))
  (^AVector [^AVector a ^AVector b & vs] 
    (let [r (- a b)]
       (doseq [^AVector v vs]
        (.sub r v))
      r)))

(defn * 
  "Multiply one or more vectors, element-wise"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [r (clone a)]
      (.multiply r b)
      r))
  (^AVector  [^AVector a ^AVector b & vs] 
    (let [r (* a b)]
      (doseq [^AVector v vs]
        (.multiply r v))
      r)))

(defn divide 
  "Divide one or more vectors, element-wise"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [r (clone a)]
      (.divide ^AVector r ^AVector b)
      r))
  (^AVector [^AVector a ^AVector b & vs] 
    (let [^AVector r (divide a b)]
      (doseq [^AVector v vs]
        (.divide r v))
      r)))