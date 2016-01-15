(ns mikera.vectorz.test-properties
  (:use clojure.core.matrix)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen :refer (sample)]
            [clojure.core.matrix.generators :as genm]
            [clojure.core.matrix.compliance-tester :as ctest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)

;; ====================================================
;; vectrorz-specific generator functions

(def gen-vectorz-arrays (genm/gen-array (genm/gen-shape) genm/gen-double (gen/return :vectorz)))


;; ====================================================
;; property based tests

(defspec generative-instance-tests 20 
  (prop/for-all [v gen-vectorz-arrays]
                (ctest/instance-test v)))

(defspec add-test 20 
  (prop/for-all [v gen-vectorz-arrays]
                (equals (mul v 2) (add v v))))

(defspec clone-test 20 
  (prop/for-all [v gen-vectorz-arrays]
                (equals v (clone v))))