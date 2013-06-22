(ns mikera.vectorz.benchmark-stats
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.stats)
  (:require [clojure.core.matrix.operators :refer [+ - *]])
  (:refer-clojure :exclude [+ - * ])
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.matrix-api])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3 Vectorz]))

(set-current-implementation :vectorz)

(defn benchmarks []
  (let [vs (vec (for [i (range 100)] (let [m (Vectorz/newVector 100)] (.set m (double i)) m)))]
    (c/quick-bench (mean vs)))
  ;; => ~2ns per element

  (let [vs (vec (for [i (range 100)] (let [m (Vectorz/newVector 100)] (.set m (double i)) m)))]
    (c/quick-bench (variance vs)))
  ;; => ~4ns per element

) 