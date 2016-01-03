(ns mikera.vectorz.test-readers
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen :refer (sample)]
            [clojure.core.matrix.generators :as genm]
            [clojure.core.matrix.compliance-tester :as ctest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)

(def gen-vectorz-arrays (genm/gen-array (genm/gen-shape) genm/gen-double (gen/return :vectorz)))

(defspec reader-round-trip 100 
  (prop/for-all [v gen-vectorz-arrays]
                (is (equals v (read-string (pr-str v))))))

