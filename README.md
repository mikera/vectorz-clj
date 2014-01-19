vectorz-clj
===========

Fast vector library for Clojure, building on the [Vectorz](https://github.com/mikera/vectorz) library and designed to work with the [core.matrix](https://github.com/mikera/core.matrix) array programming API.

`vectorz-clj` is designed so that you don't have to compromise, offering both:

 - An idiomatic high-level Clojure API using **core.matrix**
 - General purpose **multi-dimensional** arrays
 - High **performance** (about as fast as you can get on the JVM). vectorz-clj is currently the fastest pure-JVM vector/matrix library available for Clojure

The library was originally designed for games, simulations and machine learning applications, 
but should be applicable for any situations where you need numerical `double` arrays.

Important features:

 - **"Pure"** functions for an idiomatic functional programming style are provided. These return new vectors without mutating their arguments.
 - **Primitive-backed** special purpose vectors and matrices for performance, e.g. `Vector3` for fast 3D maths.
 - **Flexible DSL-style** functions for manipulating vectors and matrices, e.g. the ability to create a "view" into a subspace of a large vector.
 - **core.matrix** fully supported - see: https://github.com/mikera/core.matrix 
 - **Pure JVM code** - no native dependencies
 - **"Impure"** functions that mutate vectors are available for performance when you need it: i.e. you can use a nice functional style most of the time, but switch to mutation when you hit a bottleneck.
 
## Documentation

For more information see the [vectorz-clj Wiki](https://github.com/mikera/vectorz-clj/wiki).

### Status

`vectorz-clj` requires Clojure 1.4 or above, and an up to date version of `core.matrix`

`vectorz-clj` is reasonably stable, and implements all of the `core.matrix` API feature set. The `core.matrix` API 
is still under development, so users may expect some minor changes to the API in future releases.

[![Build Status](https://travis-ci.org/mikera/vectorz-clj.png?branch=develop)](https://travis-ci.org/mikera/vectorz-clj)

### License

Like `Vectorz`, `vectorz-clj` is licensed under the LGPL license:

 - http://www.gnu.org/licenses/lgpl.html

### Usage

Follow the instructions to install with Leiningen / Maven from Clojars: 

 - https://clojars.org/net.mikera/vectorz-clj
 
You can then use `Vectorz` as a standard `core.matrix` implementation. Example:

```clojure
    (use 'clojure.core.matrix)
    (use 'clojure.core.matrix.operators)           ;; overrides *, + etc. for matrices
    
    (set-current-implementation :vectorz)  ;; use Vectorz as default matrix implementation
    
    ;; define a 2x2 Matrix
    (def M (matrix [[1 2] [3 4]]))
    M
    => #<Matrix22 [[1.0,2.0][3.0,4.0]]>
    
    ;; define a length 2 vector (a 1D matrix is considered equivalent to a vector in core.matrix)
    (def v (matrix [1 2]))
    v
    => #<Vector2 [1.0,2.0]>
    
    ;; Matrix x Vector elementwise multiply
    (mul M v)
    => #<Matrix22 [[1.0,4.0],[3.0,8.0]]>
    
    ;; Matrix x Vector matrix multiply (inner product)
    (inner-product M v)
    => #<Vector2 [5.0,11.0]>
```

For more examples see [Wiki Examples](https://github.com/mikera/vectorz-clj/wiki/Examples)
