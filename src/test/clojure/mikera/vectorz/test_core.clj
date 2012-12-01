(ns mikera.vectorz.test-core
  (:use clojure.test)
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