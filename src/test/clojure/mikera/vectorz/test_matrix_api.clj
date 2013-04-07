(ns mikera.vectorz.test-matrix-api
  (:refer-clojure :exclude [vector? * - +])
  (:use [clojure test])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require clojure.core.matrix.compliance-tester)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz AVector Vectorz Vector]))

;; note - all the operators are core.matrix operators

(set-current-implementation :vectorz)

(deftest test-misc-regressions
  (let [v1 (v/vec1 1.0)]
    (is (array? v1))
    (is (== 1 (dimensionality v1)))
    (is (== 1 (ecount v1)))
    (is (not (matrix? v1))))
  (let [m (coerce (matrix [[1 2]]) [[1 2] [3 4]])] 
    (is (every? true? (map == (range 1 (inc (ecount m))) (eseq m)))))
  (let [m (matrix [[1 2] [3 4]])] 
    (is (== 2 (ecount (first (slices m)))))
    (scale! (first (slices m)) 2.0)
    (is (equals m [[2 4] [3 4]])))
  (let [m (matrix [[0 0] [0 0]])] 
    (assign! m [[1 2] [3 4]])
    (is (equals m [[1 2] [3 4]]))
    (assign! m [[0 0] [0 0]])
    (is (equals m [[0 0] [0 0]]))
    (mp/assign-array! m (double-array [2 4 6 8]))
    (is (equals m [[2 4] [6 8]]))
    (mp/assign-array! m (double-array 4))
    (is (equals m [[0 0] [0 0]])))
  (let [v (v/vec [1 2 3])]
    (is (equals [2 4 6] (add v v)))))

(deftest test-ndarray
  (is (equals [[[1]]] (matrix :vectorz [[[1]]])))
  (is (equals [[[[1]]]] (matrix :vectorz [[[[1]]]])))
  (is (equals [[[1]]] (slice (matrix :vectorz [[[[1]]]]) 0)))
  (is (== 4 (dimensionality (matrix :vectorz [[[[1]]]]))))
  (is (equals [[[1]]] (wrap/wrap-slice (matrix :vectorz [[[[1]]]]) 0)))
  (is (equals [[[[1]]]] (wrap/wrap-nd (matrix :vectorz [[[[1]]]]))))) 

(deftest test-element-equality
  (is (e= (matrix :vectorz [[0.5 0] [0 2]])
          [[0.5 0.0] [0.0 2.0]]))
 ;; TODO: enable this test once fixed version of core.matrix is released
 ;; (is (not (e= (matrix :vectorz [[1 2] [3 4]])
 ;;              [[5 6] [7 8]])))
  )

(deftest test-inverse
  (let [m (matrix :vectorz [[0.5 0] [0 2]])] 
    (is (equals [[2 0] [0 0.5]] (inverse m)))))

(defn test-round-trip [m]
  (is (equals m (read-string (print-str m)))))

(deftest test-round-trips
;  (test-round-trip (v/of 1 2))
;  (test-round-trip (v/of 1 2 3 4 5))
;  (test-round-trip (matrix :vectorz [[1 2 3] [4 5 6]]))
;  (test-round-trip (matrix :vectorz [[1 2] [3 4]]))
;  (test-round-trip (first (slices (v/of 1 2 3))))
)

(deftest test-equals
  (is (equals (v/of 1 2) [1 2])))

(deftest test-vector-ops
  (testing "addition"
    (is (= (v/of 1 2) (+ (v/of 1 1) [0 1])))
    (is (= (v/of 3 4) (+ (v/of 1 1) (v/of 2 3))))
    (is (= [1.0 2.0] (+ [0 2] (v/of 1 0)))))
  
  (testing "scaling"
    (is (= (v/of 2 4) (* (v/of 1 2) 2)))
    (is (= (v/of 2 4) (scale (v/of 1 2) 2)))
    (is (= (v/of 2 4) (scale (v/of 1 2) 2.0))))
  
  (testing "subtraction"
    (is (= (v/of 2 4) (- (v/of 3 5) [1 1])))
    (is (= (v/of 1 2) (- (v/of 2 3) (v/of 1 0) (v/of 0 1))))))

(deftest test-matrix-ops
  (testing "addition"
    (is (= (m/matrix [[2 2] [2 2]]) (+ (m/matrix [[1 1] [2 0]]) 
                                       (m/matrix [[1 1] [0 2]]))))
    (is (= (m/matrix [[2 2] [2 2]]) (+ (m/matrix [[1 1] [2 0]]) 
                                       [[1 1] [0 2]])))
    (is (= [[2.0 2.0] [2.0 2.0]] (+ [[1 1] [0 2]]
                                    (m/matrix [[1 1] [2 0]])))))
  (testing "scaling"
    (is (= (m/matrix [[2 2] [2 2]]) (scale (m/matrix [[1 1] [1 1]]) 2))))
  
  (testing "multiplication"
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) (m/matrix [[2] [2]]))))
    (is (= (m/matrix [[8]]) (* (m/matrix [[2 2]]) [[2] [2]])))
    ;; (is (= [[8.0]] (* [[2 2]] (m/matrix [[2] [2]]))))
    ))

(deftest test-matrix-transform
  (testing "vector multiple"
    (is (= (v/of 2 4) (* (m/matrix [[2 0] [0 2]]) (v/of 1 2))))
    (is (= (v/of 2 4) (* (m/scalar-matrix 2 2.0) (v/of 1 2))))
    (is (= (v/of 2 4) (* (m/scalar-matrix 2 2.0) [1 2]))))
  (testing "persistent vector transform"
    (is (= (v/of 1 2) (transform (m/identity-matrix 2) [1 2]))))
  (testing "transform in place"
    (let [v (matrix [1 2])
          m (matrix [[2 0] [0 2]])] 
      (transform! m v)
      (is (= (v/of 2 4) v)))))

(deftest test-slices
  (testing "slice row and column from matrix"
    (is (equals [1 2] (first (slices (matrix [[1 2] [3 4]])))))
    (is (equals [3 4] (second (slices (matrix [[1 2] [3 4]])))))
    (is (equals [3 4] (slice (matrix [[1 2] [3 4]]) 0 1)))
    (is (equals [2 4] (slice (matrix [[1 2] [3 4]]) 1 1))))
  (testing "slices of vector"
    (is (equals '(1.0 2.0 3.0) (slices (matrix [1 2 3])))))) 

;; verify scalar operators should still work on numbers!
(deftest test-scalar-operators
  (testing "addition"
    (is (== 2.0 (+ 1.0 1.0)))
    (is (== 3 (+ 1 2))))
  (testing "multiplication"
    (is (== 2.0 (* 4 0.5)))
    (is (== 6 (* 1 2 3))))
  (testing "subtraction"
    (is (== 2.0 (- 4 2.0)))
    (is (== 6 (- 10 2 2))))) 

(deftest test-construction
  (testing "1D"
    (is (= (v/of 1.0) (matrix [1])))
    (is (instance? AVector (matrix [1]))))
  (testing "2D"
    (is (= (m/matrix [[1 2] [3 4]]) (matrix [[1 2] [3 4]])))
    (is (instance? AMatrix (matrix [[1]])))))

(deftest test-conversion
  (testing "vector" 
    (is (= [1.0] (to-nested-vectors (v/of 1.0)))))
  (testing "matrix" 
    (is (= [[1.0]] (to-nested-vectors (m/matrix [[1.0]])))))
  (testing "coercion"
    (is (equals [[1 2] [3 4]] (coerce (m/matrix [[1.0]]) [[1 2] [3 4]])))))

(deftest test-functional-ops
  (testing "eseq"
    (is (= [1.0 2.0 3.0 4.0] (eseq (matrix [[1 2] [3 4]]))))))

(deftest test-maths-functions
  (testing "abs"
    (is (equals [1 2 3] (abs [-1 2 -3]))))) 

;; run compliance tests

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (v/of 1 2 3))
  (clojure.core.matrix.compliance-tester/instance-test (clone (first (slices (v/of 1 2 3)))))
  (clojure.core.matrix.compliance-tester/instance-test (first (slices (v/of 1 2 3))))
  (clojure.core.matrix.compliance-tester/instance-test (first (slices (v/of 1 2 3 4 5 6))))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[1 2] [3 4]]))) 

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (v/of 1 2))) 
