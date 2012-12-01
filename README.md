vectorz-clj
===========

Fast mutable vector library for Clojure, building on the vectorz (https://github.com/mikera/vectorz) vector maths library for Java

Specifically designed for games, simulations and machine learning.

### Purpose

vectorz-clj is a thin wrapper over vectorz, and is useful for similar reasons:

 - You want very fast, mutable vectors in Clojure
 - You want small primitive vectors for 2D/3D graphics (Vector2 and Vector3)
 - You want a DSL for vector manipulation that can do useful things like vector subranges etc.