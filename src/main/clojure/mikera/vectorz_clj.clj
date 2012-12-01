(ns mikera.vectorz-clj
  (:import [mikera.vectorz AVector Vectorz Vector]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn clone
  (^mikera.vectorz.AVector [^mikera.vectorz.AVector v]
    (.clone v)))





;; arithmetic functions

(defn + 
  "Add one or more vectors"
  ([^mikera.vectorz.AVector a] (clone a))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b] 
    (let [r (clone a)]
      (.add r b)
      r))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b & vs] 
    (let [r (clone a)]
      (.add r b)
      (doseq [^mikera.vectorz.AVector b vs]
        (.add r b))
      r)))

(defn - 
  "Substract one or more vectors"
  ([^mikera.vectorz.AVector a] (clone a))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b] 
    (let [r (clone a)]
      (.sub r b)
      r))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b & vs] 
    (let [r (clone a)]
      (.sub r b)
      (doseq [^mikera.vectorz.AVector b vs]
        (.sub r b))
      r)))

(defn * 
  "Multiply one or more vectors"
  ([^mikera.vectorz.AVector a] (clone a))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b] 
    (let [r (clone a)]
      (.multiply r b)
      r))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b & vs] 
    (let [r (clone a)]
      (.multiply r b)
      (doseq [^mikera.vectorz.AVector b vs]
        (.multiply r b))
      r)))

(defn / 
  "Divide one or more vectors"
  ([^mikera.vectorz.AVector a] (clone a))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b] 
    (let [r (clone a)]
      (.divide r b)
      r))
  ([^mikera.vectorz.AVector a ^mikera.vectorz.AVector b & vs] 
    (let [r (clone a)]
      (.divide r b)
      (doseq [^mikera.vectorz.AVector b vs]
        (.divide r b))
      r)))