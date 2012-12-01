(ns mikera.vectorz-clj
  (:import [mikera.vectorz AVector Vectorz Vector])
  (:refer-clojure :exclude [+ - * / vec vec? vector]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro error
  "Throws a vectorz error with the provided message(s)"
  ([& vals]
    `(throw (mikera.vectorz.util.VectorzException. (str ~@vals)))))


(defn clone
  (^AVector [^AVector v]
    (.clone v)))


(defn length
  "Returns the length (number of components) of a vector"
  (^long [^AVector v]
    (.length v)))

(defn vec?
  "Returns true if v is a vector (instance of mikera.vectorz.AVector)"
  ([v]
    (instance? mikera.vectorz.AVector v)))

;; vector constructors

(defn vec
  [coll]
  (cond 
    (vec? coll) (clone coll)
    (instance? java.util.List coll) (Vectorz/create ^java.util.List coll)
    (sequential? coll) 
      (let [ss (seq coll)
            len (count ss)])
    (instance? java.lang.Iterable coll) (Vectorz/create ^java.lang.Iterable coll)
    :else (error "Can't create vector from: " (class coll))))

(defn vector 
  "Creates a vector from zero or more numerical components."
  (^AVector [& xs]
    (vec xs)))

(def of vector)

(defn of-length
  "Creates a vector of a specified length. Will use optimised primitive vectors for small lengths"
  (^AVector [len]
    (Vectorz/newVector (int len))))

;; arithmetic functions

(defn + 
  "Add one or more vectors"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [r (clone a)]
      (.add r b)
      r))
  (^AVector [^AVector a ^AVector b & vs] 
    (let [r (clone a)]
      (.add r b)
      (doseq [^mikera.vectorz.AVector v vs]
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
  "Multiply one or more vectors"
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

(defn / 
  "Divide one or more vectors"
  (^AVector [^AVector a] (clone a))
  (^AVector [^AVector a ^AVector b] 
    (let [ r  (clone a)]
      (.divide ^AVector r ^AVector b)
      r))
  (^AVector [^AVector a ^AVector b & vs] 
    (let [^AVector r (/ a b)]
      (doseq [^AVector v vs]
        (.divide r v))
      r)))