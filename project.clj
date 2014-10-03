;; This file is proveded as a convenience for Leiningen users
;;
;; The pom.xml is used for official builds, and should be considered the 
;; definitive source for build configuration.

(defproject net.mikera/vectorz-clj "0.26.0"
  :description "Fast vector library for Clojure, building on Vectorz and using core.matrix"
  :url "https://github.com/mikera/vectorz-clj"
  :license {:name "GNU Lesser General Public License (LGPL)"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [criterium/criterium "0.4.3"]
                 [org.clojure/tools.analyzer "0.5.1"]
                 [org.clojure/test.check "0.5.9"]
                 [net.mikera/cljunit "0.3.1"]
                 [net.mikera/clojure-utils "0.6.1"]
                 [net.mikera/core.matrix "0.29.1"]
                 [net.mikera/core.matrix.stats "0.4.0"]
                 [net.mikera/vectorz "0.40.0"]]
  :repositories [["clojars.org" {:url "https://clojars.org/repo"
                                 :name "Clojars repository"}]])
