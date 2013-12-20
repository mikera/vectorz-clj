(defproject net-mikera/vectorz-clj "0.17.1-SNAPSHOT"
  :description "Fast vector library for Clojure, building on VectorZ and using core.matrix"
  :url "https://github.com/mars0i/vectorz-clj"
  :license {:name "GNU Lesser General Public License (LGPL)"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :source-paths ["src/main/clojure"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium/criterium "0.4.2"]
                 [net.mikera/cljunit "0.3.0"]
                 [net.mikera/clojure-utils "0.5.0"]
                 ;[net.mikera/core.matrix "0.15.0"]
                 [mars0i/core.matrix "0.16.1-SNAPSHOT"]
                 ;[net.mikera/core.matrix.stats "0.3.0"] ; references earlier core.matrix
                 [net.mikera/vectorz "0.23.0"]]
  :repositories [["clojars.org" {:url "https://clojars.org/repo"
                                 :name "Clojars repository"}]])
