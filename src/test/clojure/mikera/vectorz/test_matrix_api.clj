(ns mikera.vectorz.test-matrix-api
  (:refer-clojure :exclude [vector? * - +])
  (:use [clojure test])
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :refer [+ - *]])
  (:require clojure.core.matrix.compliance-tester)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:require [mikera.vectorz.matrix-api])
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:import [mikera.matrixx AMatrix Matrixx Matrix])
  (:import [mikera.vectorz Scalar])
  (:import [mikera.vectorz AVector Vectorz Vector])
  (:import [mikera.arrayz INDArray Array NDArray SliceArray]))

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
    (is (equals [2 4 6] (add v v))))
  (let [v (Vector/of (double-array 0))]
    (is (== 10 (reduce (fn [acc _] (inc acc)) 10 (eseq v))))
    (is (== 10 (ereduce (fn [acc _] (inc acc)) 10 v)))))

(deftest test-scalar-add
  (is (equals [2 3 4] (add 1 (array :vectorz [1 2 3]))))
  (is (equals [2 3 4] (add (array :vectorz [1 2 3]) 1 0)))) 

(deftest test-ecount
  (is (== 1 (ecount (Scalar. 10))))
  (is (== 2 (ecount (v/of 1 2))))
  (is (== 0 (ecount (Vector/of (double-array 0)))))
  (is (== 0 (count (eseq (Vector/of (double-array 0))))))
  (is (== 0 (ecount (coerce :vectorz []))))
  (is (== 4 (ecount (coerce :vectorz [[1 2] [3 4]]))))
  (is (== 8 (ecount (coerce :vectorz [[[1 2] [3 4]] [[1 2] [3 4]]]))))) 

(deftest test-mutability
  (let [v (v/of 1 2)]
    (is (mutable? v))
    (is (mutable? (first (slices v)))))
  (let [v (new-array :vectorz [3 4 5 6])]
    (is (v/vectorz? v))
    (is (mutable? v))
    (is (mutable? (first (slices v))))))

(deftest test-new-array
  (is (instance? AVector (new-array :vectorz [10])))
  (is (instance? AMatrix (new-array :vectorz [10 10])))
  (is (instance? INDArray (new-array :vectorz [3 4 5 6])))) 

(deftest test-sub
  (let [a (v/vec [1 2 3 0 0])
        b (v/vec [1 1 4 0 0])]
    (is (equals [0 1 -1 0 0] (sub a b))))) 

(deftest test-add-product
  (let [a (v/vec [1 2])
        b (v/vec [1 1])]
    (is (equals [2 5] (add-product b a a)))
    (is (equals [3 9] (add-scaled-product b a [1 2] 2)))
    (is (equals [11 21] (add-product b a 10)))
    (is (equals [11 21] (add-product b 10 a))))) 

(deftest test-add-product!
  (let [a (v/vec [1 2])
        b (v/vec [1 1])]
    (add-product! b a a)
    (is (equals [2 5] b))
    (add-scaled! b a -1)
    (is (equals [1 3] b))
    (add-scaled-product! b [0 1] [3 4] 2)
    (is (equals [1 11] b)))) 


(deftest test-coerce
  (let [a (v/vec [1 2 3 0 0])
        b (v/vec [1 1 4 0 0])
        r (sub a b)]
    (is (equals [0 1 -1 0 0] (coerce [] r)))
    (is (instance? clojure.lang.IPersistentVector (coerce [] r)))
    ;; (is (instance? INDArray (coerce :vectorz 10.0))) ;; TODO: what should this be??
    )) 

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

(deftest test-det
  (is (== -1.0 (det (matrix :vectorz [[0 1] [1 0]])))))

(defn test-round-trip [m]
  (is (equals m (read-string (.toString m))))
  ;; TODO edn round-tripping?
  )

(deftest test-round-trips
  (test-round-trip (v/of 1 2))
  (test-round-trip (v/of 1 2 3 4 5))
  (test-round-trip (matrix :vectorz [[1 2 3] [4 5 6]]))
  (test-round-trip (matrix :vectorz [[1 2] [3 4]]))
  (test-round-trip (first (slices (v/of 1 2 3))))
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
    (is (= (v/of 2 4) (scale (v/of 1 2) 2N)))
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

(deftest test-join
  (is (= (array [[[1]] [[2]]]) (join (array [[[1]]]) (array [[[2]]])))))

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
    (is (= [1.0] (to-nested-vectors (v/of 1.0))))
    (is (= [1.0] (coerce [] (v/of 1.0)))))
  (testing "matrix" 
    (is (= [[1.0]] (to-nested-vectors (m/matrix [[1.0]])))))
  (testing "coercion"
    (is (equals [[1 2] [3 4]] (coerce (m/matrix [[1.0]]) [[1 2] [3 4]])))
    (is (number? (coerce :vectorz 10)))
    (is (instance? AVector (coerce :vectorz [1 2 3])))
    (is (instance? AMatrix (coerce :vectorz [[1 2] [3 4]])))))

(deftest test-functional-ops
  (testing "eseq"
    (is (= [1.0 2.0 3.0 4.0] (eseq (matrix [[1 2] [3 4]]))))
    (is (empty? (eseq (coerce :vectorz []))))  
    (is (= [10.0] (eseq (array :vectorz 10))))  
    (is (= [10.0] (eseq (array :vectorz [[[10]]]))))  
    (is (== 1 (first (eseq (v/of 1 2))))))
  (testing "emap"
    (is (equals [1 2] (emap inc (v/of 0 1))))
    (is (equals [1 3] (emap + (v/of 0 1) [1 2])))
    ;; (is (equals [2 3] (emap + (v/of 0 1) 2))) shouldn't work - no broadcast support in emap?
    (is (equals [3 6] (emap + (v/of 0 1) [1 2] (v/of 2 3))))))

(deftest test-maths-functions
  (testing "abs"
    (is (equals [1 2 3] (abs [-1 2 -3])))
    (is (equals [1 2 3] (abs (v/of -1 2 -3)))))) 

(deftest test-assign
  (is (e== [2 2] (assign (v/of 1 2) 2)))
  (let [m (array :vectorz [1 2 3 4 5 6])]
    (is (e== [1 2 3] (subvector m 0 3)))
    (is (e== [4 5 6] (subvector m 3 3)))
    (assign! (subvector m 0 3) (subvector m 3 3))
    (is (e== [4 5 6 4 5 6] m)))) 

;; vectorz operations hould return a vectorz datatype
(deftest test-vectorz-results
  (is (v/vectorz? (+ (v/of 1 2) [1 2])))
  (is (v/vectorz? (+ (v/of 1 2) 1)))
  (is (v/vectorz? (- 2 (v/of 1 2))))
  (is (v/vectorz? (* (v/of 1 2) 2.0)))
  (is (v/vectorz? (emap inc (v/of 1 2))))
  (is (v/vectorz? (array [[[1]]])))
  (is (v/vectorz? (to-vector (array [[[1]]]))))
  (is (v/vectorz? (identity-matrix 3)))
  (is (v/vectorz? (reshape (identity-matrix 3) [5 1])))
  (is (v/vectorz? (slice (identity-matrix 3) 1)))
  (is (v/vectorz? (* (identity-matrix 3) [1 2 3])))
  (is (v/vectorz? (inner-product (v/of 1 2) [1 2])))
  (is (v/vectorz? (outer-product (v/of 1 2) [1 2])))
  (is (v/vectorz? (add! (Scalar. 1.0) 10)))) 

;; run compliance tests

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (Scalar. 2.0))
  (clojure.core.matrix.compliance-tester/instance-test (v/of 1 2))
  (clojure.core.matrix.compliance-tester/instance-test (v/of 1 2 3))
  (clojure.core.matrix.compliance-tester/instance-test (v/of 1 2 3 4 5 6 7))
  (clojure.core.matrix.compliance-tester/instance-test (subvector (v/of 1 2 3 4 5 6 7) 2 3))
  (clojure.core.matrix.compliance-tester/instance-test (matrix :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]]))
  (clojure.core.matrix.compliance-tester/instance-test (clone (first (slices (v/of 1 2 3)))))
  (clojure.core.matrix.compliance-tester/instance-test (first (slices (v/of 1 2 3))))
;;  (clojure.core.matrix.compliance-tester/instance-test (Vector/of (double-array 0))) ;; TODO: needs fixed compliance tests
  (clojure.core.matrix.compliance-tester/instance-test (first (slices (v/of 1 2 3 4 5 6))))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[1 2] [3 4]]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[[[4]]]]))
  (clojure.core.matrix.compliance-tester/instance-test (Array/create (array :vectorz [[[[4 3]]]])))) 

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (v/of 1 2))) 
