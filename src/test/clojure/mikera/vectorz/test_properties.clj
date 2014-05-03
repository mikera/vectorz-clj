(ns mikera.vectorz.test-properties
  (:use clojure.core.matrix)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)])
  (:import [mikera.vectorz AVector Vectorz Vector]))