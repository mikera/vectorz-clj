(ns mikera.vectorz.generators
  (:require [clojure.core.matrix :as m]
            [clojure.test.check.generators :as gen]
            [clojure.core.matrix.generators :as gm]
            [clojure.test.check.properties :as prop]
            [mikera.cljutils.error :refer [error]]))

(defn subvector
  "Creates a subvector generator from a vector generator"
  ([g-vector]
    (gen/bind g-vector
              (fn [v]
                (let [n (m/ecount v)]
                  ())))))

(defn mutable-array
  "Create a generator for fully mutable arrays"
  ([]
    (gen/one-of 
      [(gm/gen-array (gm/gen-shape) gm/gen-double :vectorz)])))

(defn mutable-vector 
  "Create a generator for fully mutable vectors
"
  ([]
    (gen/one-of 
      [(gm/gen-vector gm/gen-double :vectorz)
       (gen/fmap m/as-vector (mutable-array))])))

