(ns mikera.vectorz.test-core
  (:use clojure.test)
  (:require mikera.vectorz.examples) 
  (:require [mikera.vectorz.core :as v])
  (:import [mikera.vectorz AVector Vectorz Vector]))


(deftest test-arithmetic
  (testing "addition"
    (is (= (v/of 1 2) 
           (v/+ (v/of 0 2) (v/of 1 0)))))
  (testing "subtraction"
    (is (= (v/of 1 2) 
           (v/- (v/of 3 3) (v/of 2 1)))))
  (testing "division"
    (is (= (v/of 1 2) 
           (v/divide (v/of 10 10) (v/of 10 5)))))
  (testing "multiplication"
    (is (= (v/of 2 6) 
           (v/* (v/of 1 2) (v/of 2 3))))))

(deftest test-vector-ops
  (testing "dot product"
    (is (= 10.0 (v/dot (v/of 2 3 1) (v/of 1 2 2)))))
  (testing "distance"
    (is (= 5.0 (v/distance (v/of 1 0 0) (v/of 1 3 4))))))

(deftest test-more-vector-ops
  (testing "add weighted"
    (is (= (v/of 2 6) 
           (v/add-weighted (v/of 1 5) (v/of 5 9) 1/4))))
  (testing "interpolate"
    (is (= (v/of 2 6) 
           (v/interpolate (v/of 5 9) (v/of 1 5)  3/4)))))

(deftest test-get-set
  (testing "get"
    (is (= 10.0 (v/get (v/of 5 10 15) 1))))
  (testing "set"
    (is (= (v/of 5 10 15) 
           (v/set (v/of 5 0 15) (long 1) 10.0)))
    (is (= (v/of 3 2) (v/to-vector (v/set (v/of 1 2) 0 3))))))

(deftest test-seq
  (testing "to seq"
    (is (= [2.0 3.0] (seq (v/of 2 3))))))

(deftest test-refs
  (testing "join"
    (let [v1 (v/of 1 2)
          v2 (v/of 3 4)
          jv (v/join v1 v2)] 
      (is (= (v/of 1 2 3 4) jv))
      (v/fill! (v/subvec jv 1 2) 10)
      (is (== 10.0 (v/get v1 1)))
      (is (== 10.0 (v/get v2 0)))
      (v/fill! (v/clone jv) 20)
      (is (not= 20.0 (v/get v2 0))))))

(deftest test-assign
  (testing "assign"
    (is (= (v/of 1 2) (v/assign! (v/of 2 3) (v/of 1 2))))))

(deftest test-construction
  (testing "from double arrays"
    (is (= (v/of 1 2 3) (v/vec (double-array [1 2 3]))))))

(deftest test-primitive-vector-constructors
  (testing "vec1"
    (is (= (v/of 1) (v/vec1 1) ))
    (is (= (v/of 1) (v/vec1 [1])))
    (is (= (v/of 0) (v/vec1)))
    (is (thrown? Throwable (v/vec1 [2 3]))))
  (testing "vec2"
    (is (= (v/of 1 2) (v/vec2 1 2) ))
    (is (= (v/of 1 2) (v/vec2 [1 2])))
    (is (= (v/of 0 0) (v/vec2)))
    (is (thrown? Throwable (v/vec2 [2]))))
  (testing "vec3"
    (is (= (v/of 1 2 3) (v/vec3 1 2 3) ))
    (is (= (v/of 1 2 3) (v/vec3 [1 2 3])))
    (is (= (v/of 0 0 0) (v/vec3)))
    (is (thrown? Throwable (v/vec3 [2 3]))))
  (testing "vec4"
    (is (= (v/of 1 2 3 4) (v/vec4 1 2 3 4) ))
    (is (= (v/of 1 2 3 4) (v/vec4 [1 2 3 4])))
    (is (= (v/of 0 0 0 0) (v/vec4)))
    (is (thrown? Throwable (v/vec4 [2 3])))))