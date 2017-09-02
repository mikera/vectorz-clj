(ns mikera.vectorz.test-ops
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:import [mikera.vectorz AVector Vectorz Vector]
           [mikera.vectorz Ops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(deftest test-vector-ops
  (testing "Vectorz Op"
    (is (equals [1 2] (emap Ops/ABS (array :vectorz [1 -2]))))))

(deftest test-add-emap
  (testing "Vectorz Op"     
    (let [dest (array :vectorz [10 100])]
      (is (equals [14 106] (add-emap! dest Ops/ADD (array :vectorz [1 2]) [3 4]))))))

(deftest test-set-emap
  (testing "Vectorz Op"     
    (let [dest (array :vectorz [10 100])]
      (is (equals [4 6] (set-emap! dest Ops/ADD (array :vectorz [1 2]) [3 4]))))))
