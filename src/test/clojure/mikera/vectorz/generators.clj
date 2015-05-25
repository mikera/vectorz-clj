(ns mikera.vectorz.generators
  (:require [clojure.core.matrix :as m]
            [clojure.test.check.generators :as gen]
            [clojure.core.matrix.generators :as gm]
            [clojure.test.check.properties :as prop]
            [mikera.cljutils.error :refer [error]]))

(defn mutable-vector 
  "Create a generator for fully mutable matrices of the gien shape"
  ([]
    (gen/bind (gen/choose 1 1) 
             (fn [i] 
               (case i 
                       1 (gm/gen-vector gm/gen-double :vectorz)
                       (error "Incorrect generator type: " i))))))

