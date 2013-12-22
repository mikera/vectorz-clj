(ns test.misc.loading
  (:use [clojure.core.matrix])
  (:use [clojure.core.matrix.utils]))

(defn foo []
  (doall (map deref (for [i (range 10)] (future (matrix :vectorz [0 1]))))))