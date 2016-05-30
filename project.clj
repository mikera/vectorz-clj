;; This file is proveded as a convenience for Leiningen users
;;
;; The pom.xml is used for official builds, and should be considered the 
;; definitive source for build configuration.
;;
;; If you are having trouble building, please check the pom.xml for latest dependency versions

(defproject net.mikera/vectorz-clj "0.30.2-SNAPSHOT"
  :description "Fast vector library for Clojure, building on Vectorz and using core.matrix"
  :url "https://github.com/mikera/vectorz-clj"
  :license {:name "GNU Lesser General Public License (LGPL)"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha3"]
                 [criterium/criterium "0.4.4"]
                 [org.clojure/tools.analyzer "0.6.7"]
                 [org.clojure/test.check "0.9.0"]
                 [net.mikera/cljunit "0.4.1"]
                 [net.mikera/clojure-utils "0.7.0"]
                 [net.mikera/core.matrix "0.52.0"]
                 ;; [net.mikera/core.matrix "0.47.1" :type "test-jar"] ;; bug in Lein!!!! see: https://github.com/technomancy/leiningen/issues/1975
                 [net.mikera/vectorz "0.62.0"]]
  
  :profiles {:dev {:java-source-paths ["src/test/java"]}}
  
  :repositories [["clojars.org" {:url "https://clojars.org/repo"
                                 :name "Clojars repository"}]])
