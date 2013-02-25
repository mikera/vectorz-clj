vectorz-clj
===========

Fast vector library for Clojure, building on the vectorz library (https://github.com/mikera/vectorz).

Specifically designed for games, simulations and machine learning. 

Specific features that may be appealing:

 - **"Pure"** functions for idiomatic Clojure style are provided. These work as you expect, i.e. return new vectors without mutating their arguments.
 - **"Impure"** functions that mutate vectors are available for performance when you need it. i.e. you can use a nice functional style most of the time, but switch to mutation when you hit a bottleneck.
 - **Primitive-backed** special purpose vectors and matrices for performance, e.g. Vector3 for fast 3D maths.
 - **Flexible DSL-style** functions for manipulating vectors and matrices, e.g. the ability to create a "view" into a subspace of a large vector.
 - **`core.matrix`** fully supported
 
[![Build Status](https://travis-ci.org/mikera/vectorz-clj.png?branch=vectorz-clj-0.2.2)](https://travis-ci.org/mikera/vectorz-clj)

### License

Like vectorz, vectorz-clj is licensed under the LGPL license:

 - http://www.gnu.org/licenses/lgpl.html

### Usage

Follow the instructions to install with Leiningen / Maven from Clojars: 

 - https://clojars.org/net.mikera/vectorz-clj
 
You can then use Vectorz as a standard core.matrix implementation. Example:

```clojure
    (use 'clojure.core.matrix)
    (use 'clojure.core.matrix.operators)           ;; overrides *, + etc. for matrices
    
    (set-current-implementation :vectorz)  ;; uses Vectorz as default matrix implementation
    
    ;; define a 2x2 Matrix
    (def M (matrix [[1 2] [3 4]]))
    M
    => #<Matrix22 [[1.0,2.0][3.0,4.0]]>
    
    ;; define a length 2 vector (a 1D matrix is equivalent considered a vector in core.matrix)
    (def v (matrix [1 2]))
    v
    => #<Vector2 [1.0,2.0]>
    
    ;; Matrix x Vector multiply
    (* M v)
    => #<Vector2 [5.0,11.0]>
```

Vectorz also provides specialized functions in `mikera.vectorz.core` that give access to 
advanced features of Vectorz that are not necessarily available through the core.matrix API.
This includes type hinted and primitive functions that will perform better than using 
the general purpose core.matrix API.

Here are some Vector Examples:

```clojure
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
    
    ;; you can even get references to sub-vectors and mutate them
    (let [v (create-length 10)]   ;; create a vector of 10 zeros
      (fill! (subvec v 3 4) 1.0)  ;; fill a subvector with ones
      v)
    => #<Vector [0.0,0.0,0.0,1.0,1.0,1.0,1.0,0.0,0.0,0.0]>
```
    
### Purpose

vectorz-clj design goals:

 - Pure JVM code (i.e. no native dependencies)
 - Very fast, mutable vectors ("as fast as you can get on the JVM")
 - Build upon a good abstraction for vectors (AVector)
 - Different concrete vector types, e.g. primitive vectors for 2D/3D graphics (Vector2 and Vector3)
 - DSL for vector manipulation that can do useful things like vector subranges etc.
 - Matrix functionality as required to complement the vector types
 
vectorz-clj is implemented as a "Clojurized" wrapper over the vectorz library, which provides the underlying data structures as well as a set of APIs that can be used from Java.