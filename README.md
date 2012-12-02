vectorz-clj
===========

Fast mutable vector library for Clojure, building on the vectorz library (https://github.com/mikera/vectorz).

Specifically designed for games, simulations and machine learning.

### Usage

Follow the instructions to install with Leiningen / Maven from Clojars: https://clojars.org/net.mikera/vectorz-clj

Examples:

    (in-ns 'mikera.vectorz.core)

    (def a (vec [1 2 3]))

    (+ a a)                    ;; standard arithmetic operations 
    => #<Vector [2.0,4.0,6.0]>
    
    (join a (vec [4 5 6]))     ;; vector concatenation
    => #<JoinedVector [1.0,2.0,3.0,4.0,5.0,6.0]>

    (magnitude a)              ;; vector magnitude (length)
    => 3.7416573867739413
    
    (add! a (vec [2 2 2]))     ;; in-place vector mutation
    a
    => #<Vector [3.0,4.0,5.0]>
    
### Purpose

vectorz-clj design goals:

 - Pure JVM code (i.e. no native dependencies)
 - Very fast, mutable vectors ("as fast as you can get on the JVM")
 - Different concrete vector types, e.g. primitive vectors for 2D/3D graphics (Vector2 and Vector3)
 - DSL for vector manipulation that can do useful things like vector subranges etc.
 
 vectorz-clj is implemented as a wrapper over the vectorz library, which provides the underlying data structures as well as a set of APIs that can be used from Java.