(ns mikera.vectorz.test-properties
  (:use clojure.core.matrix)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen :refer (sample)]
            [clojure.core.matrix.generators :as genm]
            [clojure.core.matrix.compliance-tester :as ctest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(set-current-implementation :vectorz)

(def gen-vectorz-arrays (genm/gen-array (genm/gen-shape) genm/gen-double (gen/return :vectorz)))

(defspec first-element-is-min-after-sorting 20 
  (prop/for-all [v gen-vectorz-arrays]
                (ctest/instance-test v)))