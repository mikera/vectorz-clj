(ns mikera.vectorz.test-properties
  (:use clojure.core.matrix)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)])
  (:import [mikera.vectorz AVector Vectorz Vector]))

(set-current-implementation :vectorz)

;; generator for legitimate shapes for vectors
;; vector sizes grow linearly
(def gen-vector-shape
  (gen/fmap vector gen/pos-int))

;; generator for legitimate shapes of arrays. We don't allow zero-length dimensions
;; element counts grow linearly
(def gen-shape
  :TODO)