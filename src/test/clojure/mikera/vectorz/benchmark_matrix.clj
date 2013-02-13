(ns mikera.vectorz.benchmark-matrix
  (:use core.matrix)
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3]))

(defn benchmarks []
  ;; direct vectorz add
  (let [^Vector3 a ( v/vec [1 2 3])
        ^Vector3 b ( v/vec [1 2 3])]
    (c/quick-bench (dotimes [i 1000] (.add a b))))
  
   ;; core.matrix add
  (let [a (v/vec [1 2 3])
        b (v/vec [1 2 3])]
    (c/quick-bench (dotimes [i 1000] (add a b))))  
    
  ;; direct persistent vector add
  (let [a [1 2 3]
        b [1 2 3]]
    (c/quick-bench (dotimes [i 1000] (mapv + a b))))  
  
  ;; persistent vector core.matrix add
  (let [a [1 2 3]
        b [1 2 3]]
    (c/quick-bench (dotimes [i 1000] (add a b))))  
  

) 