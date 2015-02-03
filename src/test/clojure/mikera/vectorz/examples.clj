(ns mikera.vectorz.examples
  (:refer-clojure :exclude [+ - *])
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :refer [+ - *]])
  (:require mikera.vectorz.matrix-api))

;; in which we run a sequence of examples to demonstrate vectorz-clj features

(defn example []

;; first up, tell core.matrix that we want to use vectorz as our default implementation
(set-current-implementation :vectorz)

;; create a new 3D vector
(def a (new-vector 3))
a
;; => #<Vector3 [0.0,0.0,0.0]>

;; check the class of our vector. it should be a Java class from the Vectorz packages
(class a)
;; => mikera.vectorz.Vector3


;; convert a vectorz vector into a regular Clojure persistent vector
;; our vector should be empty at the moment (all zeros)
(coerce [] a)
;; => [0.0 0.0 0.0]


;; assign to a vector using core.matrix functions
;;
;; Note 1: you can use clojure vectors quite happily as arguments to core.matrix functions: 
;; they are considered as valid matrices by core.matrix
;; Note 2: functions with a ! cause mutation
(assign! a [1 2 3])
;; => #<Vector3 [1.0,2.0,3.0]>


;; create a normalised version of our vector
(def n (normalise a))
n
;; => #<Vector3 [0.2672612419124244,0.5345224838248488,0.8017837257372732]>

;; normalised vector should have a length of 1.0, or very close (subject to numerical error)
(length n)
;;=> 1.0 



)