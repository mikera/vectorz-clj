(ns mikera.vectorz.benchmark-matrix
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:refer-clojure :exclude [+ - *])
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3 Vectorz]))

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
  
  (let [a [1 2 3 4 5 6 7 8 9 10]
        b [1 2 3 4 5 6 7 8 9 10]]
    (c/quick-bench (dotimes [i 1000] (vec (map clojure.core/+ a b)))))  
  ;; => Execution time mean per addition : 1308 ns
  
  (let [a (matrix :vectorz [1 2 3 4 5 6 7 8 9 10])
        b (matrix :vectorz [1 2 3 4 5 6 7 8 9 10])]
    (c/quick-bench (dotimes [i 1000] (+ a b))))
  ;; => Execution time mean per addition: 68 ns
  
  (let [a (matrix :vectorz [1 2 3 4 5 6 7 8 9 10])
        b (matrix :vectorz [1 2 3 4 5 6 7 8 9 10])]
    (c/quick-bench (dotimes [i 1000] (add! a b))))
  ;; => Execution time mean per addition: 36 ns
  
  (let [a (Vectorz/create [1 2 3 4 5 6 7 8 9 10])
        b (Vectorz/create [1 2 3 4 5 6 7 8 9 10])]
    (c/quick-bench (dotimes [i 1000] (.add a b))))
  ;; => Execution time mean per addition: 11 ns

) 