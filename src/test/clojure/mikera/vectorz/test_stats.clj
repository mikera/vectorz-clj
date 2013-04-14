(ns mikera.vectorz.test-stats
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.stats)
  (:use clojure.test)
  (:require [mikera.vectorz.core :as v]))

(deftest test-mean
  (let [vs (map v/vec [[1 2] [3 4] [5 6] [7 8]])]
    (is (e== [4 5] (mean vs)))))

