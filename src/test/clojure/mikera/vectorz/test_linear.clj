(ns mikera.vectorz.test-linear
  (:use [clojure.test]
        [clojure.core.matrix]
        [clojure.core.matrix.linear])
  (:require [mikera.vectorz.core :as v]))

(set-current-implementation :vectorz)

(deftest test-svd
  (let [result (svd [[2 0] [0 1]])]
    (is (every? v/vectorz? (vals result)))
    (is (equals [2 1] (:S result)))
    (is (every? orthogonal? ((juxt :V* :U) result)))))

(deftest test-qr
  (let [A (matrix [[2 0] [0 1]])
        result (qr A)
        R (:R result)
        Q (:Q result)]
    (is (orthogonal? Q))
    (is (upper-triangular? R))
    (is (equals A (mmul Q R)))))

;; TODO: Reinstate once linear algebra implementation complete
;(deftest test-solve
;  (let [A [[1 2]
;           [2 1]]
;        b [22
;           26]]
;    (is (equals [10 6] (solve A b)))))


(deftest test-QR-decomposition
  (let [epsilon 0.00001]
    (testing "test0"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [Q R]} (qr M {:return [:Q]})]
        (is (orthogonal? Q))
        (is (and Q (not R)))))
    (testing "test1"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [Q R]} (qr M {:return [:Q :R]})]
        (is (orthogonal? Q))
        (is (upper-triangular? R))
        (is (equals M (mmul Q R) epsilon))))
    (testing "test2"
      (let [M (matrix [[111 222 333][444 555 666][777 888 999]])
            {:keys [Q R]} (qr M nil)]
        (is (orthogonal? Q))
        (is (upper-triangular? R))
        (is (equals M (mmul Q R) epsilon))))
    (testing "test3"
      (let [M (matrix [[-1 2 0][14 51 6.23][7.1242 -8.4 119]])
            {:keys [Q R]} (qr M)]
        (is (orthogonal? Q))
        (is (upper-triangular? R))
        (is (equals M (mmul Q R) epsilon))))))

 (deftest test-QR-decomposition-rectangular
   (let [epsilon 0.00001]
     (testing "should decompose wide matrices"
       (let [M (matrix [[1 2 3 4 5][6 7 8 9 10][11 12 13 14 15]])
             {:keys [Q R]} (qr M)]
         (is (= [3 3](shape Q)))
         (is (orthogonal? Q))
         (is (= [3 5](shape R)))
         (is (equals M (mmul Q R) epsilon))))
     (testing "should decompose tall matrices"
       (let [M (matrix [[1 2 3][4 5 6][7 8 9][10 11 12][13 14 15]])
             {:keys [Q R]} (qr M)]
         (is (= [5 5](shape Q)))
         (is (orthogonal? Q))
         (is (= [5 3](shape R)))
         (is (equals M (mmul Q R) epsilon))))))

(deftest test-LUP-decomposition
  (let [epsilon 0.00001]
    (testing "test0"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [L U P]} (lu M {:return [:U]})]
        (is (upper-triangular? U))
        (is (and U (not L) (not P)))))
    (testing "test1"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [L U P]} (lu M {:return [:L :U :P]})
            p (matrix [[0.0 1.0 0.0][0.0 0.0 1.0][1.0 0.0 0.0]])]
        (is (lower-triangular? L))
        (is (upper-triangular? U))
        (is (equals P p epsilon))
        (is (equals M (mmul P L U) epsilon))))
    (testing "test2"
      (let [M (matrix [[76 87 98][11 21 32][43 54 65]])
            {:keys [L U P]} (lu M)
            p (matrix [[1.0 0.0 0.0][0.0 1.0 0.0][0.0 0.0 1.0]])]
        (is (lower-triangular? L))
        (is (upper-triangular? U))
        (is (equals P p epsilon))
        (is (equals M (mmul P L U) epsilon))))))

(deftest test-SVD-decomposition
  (let [epsilon 0.00001]
    (testing "test0"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [U S V*]} (svd M {:return [:S]})]
        (is (and S (not U) (not V*)))))
    (testing "test1"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            {:keys [U S V*]} (svd M {:return [:U :S :V*]})
            S_matrix (diagonal-matrix :vectorz S)]
        (is (orthogonal? U))
        (is (orthogonal? V*))
        (is (equals M (mmul U S_matrix V*) epsilon))))
    (testing "test2"
      (let [M (matrix [[12 234 3.23][-2344 -235 61][-7 18.34 9]])
            {:keys [U S V*]} (svd M)
            S_matrix (diagonal-matrix :vectorz S)]
        (is (orthogonal? U))
        (is (orthogonal? V*))
        (is (equals M (mmul U S_matrix V*) epsilon))))
    (testing "test3"
      (let [M (matrix [[76 87 98][11 21 32][43 54 65]])
            {:keys [U S V*]} (svd M nil)
            S_matrix (diagonal-matrix :vectorz S)]
        (is (orthogonal? U))
        (is (orthogonal? V*))
        (is (equals M (mmul U S_matrix V*) epsilon))))))

(deftest test-Cholesky-decomposition
  (let [epsilon 0.00001]
    (testing "test0"
      (let [M (matrix [[1 2 3][4 5 6][7 8 9]])
            result (cholesky M)]
        (is (nil? result))))
    (testing "test1"
      (let [M (matrix [[4 12 -16][12 37 -43][-16 -43 98]])
            {:keys [L L*]} (cholesky M {:return [:L]})]
        (is (lower-triangular? L))
        (is (and L (not L*)))
        (is (equals M (mmul L (transpose L)) epsilon))
        (is (reduce (fn [a b] (and a (> b 0))) true (diagonal L)))))
    (testing "test1"
      (let [M (matrix [[2 -1 0][-1 2 -1][0 -1 2]])
            {:keys [L L*]} (cholesky M {:return [:L :L*]})]
        (is (lower-triangular? L))
        (is (upper-triangular? L*))
        (is (equals L (transpose L*) epsilon))
        (is (equals M (mmul L L*) epsilon))
        (is (reduce (fn [a b] (and a (> b 0))) true (diagonal L)))
        (is (reduce (fn [a b] (and a (> b 0))) true (diagonal L*)))))))

(deftest test-norm
  (let [M (matrix [[1 2 3][4 5 6][7 8 9]])]
    (is (equals 45.0 (norm M 1) 1e-10))
    (is (equals 16.88194301613 (norm M 2) 1e-10))
    (is (equals 9 (norm M java.lang.Double/POSITIVE_INFINITY) 1e-10))
    (is (equals 16.88194301613 (norm M) 1e-10))
    (is (equals 12.65148997952 (norm M 3) 1e-10))
    (let [V (.asVector M)]
      (is (equals 45.0 (norm V 1) 1e-10))
      (is (equals 16.88194301613 (norm V 2) 1e-10))
      (is (equals 9 (norm V java.lang.Double/POSITIVE_INFINITY) 1e-10))
      (is (equals 16.88194301613 (norm V) 1e-10))
      (is (equals 0.941944314533 (norm V -3) 1e-10))
      (is (equals 12.65148997952 (norm V 3) 1e-10)))))

(deftest test-rank
  (let [M1 (matrix [[1 2 3][4 5 6][7 8 9]])
        M2 (identity-matrix 3)
        M3 (matrix [[1 1 1][1 1 1][1 1 1]])]
    (is (equals 2 (rank M1)))
    (is (equals 3 (rank M2)))
    (is (equals 1 (rank M3)))))

(deftest test-solve
  (let [M1 (matrix [[1 -2 1][0 1 6][0 0 1]])
        M2 (matrix [[1 2 3][4 5 6][7 8 9]])    ;Singular matrix
        V1 (array [4 -1 2])
        V2 (vec [4 -1 2])
        A1 (array [-24 -13 2])]
    (is (equals A1 (solve M1 V1) 1e-8))
    (is (equals A1 (solve M1 V2) 1e-8))
    (is (nil? (solve M2 V1)))))

(deftest test-least-squares
  (let [M1 (matrix [[1 2][3 4][5 6]])
        V1 (array [1 2 3])
        V2 (vec [1 2 3])
        A1 (array [0 0.5])]
    (is (equals A1 (least-squares M1 V1) 1e-8))
    (is (equals A1 (least-squares M1 V2) 1e-8))))
