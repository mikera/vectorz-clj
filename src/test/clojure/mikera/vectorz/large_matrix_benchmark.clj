(ns mikera.vectorz.large-matrix-benchmark
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :refer [+ - *]])
  (:refer-clojure :exclude [+ - *])
  (:require [criterium.core :as c])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [mikera.vectorz Vector3 Vectorz]))

(set-current-implementation :vectorz)

(defn benchmarks []
  ;; direct vectorz add
  (let [a (matrix (range 1000))
        b (matrix (range 1000))]
    (c/quick-bench (dotimes [i 1000] (+ a b))))
  ;; 2437 ns per add  

  ;; 100x100 matrix construction
  (let []
    (c/quick-bench (matrix (map (fn [r] (range 100)) (range 100)))))
  ;; 45 ns per element
  
  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (+ m m))) 
  ;; 10 ns per element - OK-ish
  
  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (* m m))) 
  ;; 1.3ns per multiply??
  
  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (mul m m))) 
  ;; 1.3ns per multiply??
  
  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (abs! m))) 
  ;; ~1.0ns per element
  
  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (sqrt! m))) 
  ;; ~3.4ns per element

  (let [m (matrix (map (fn [r] (range 100)) (range 100)))]
    (c/quick-bench (cbrt! m))) 
  ;; ~19ns per element


) 