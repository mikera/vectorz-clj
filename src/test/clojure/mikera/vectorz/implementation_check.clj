(ns mikera.vectorz.implementation-check
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.utils :as utils]))

(set-current-implementation :vectorz)

(defn test-impls []
	(array [1])
	
	(def protos (utils/extract-protocols)))