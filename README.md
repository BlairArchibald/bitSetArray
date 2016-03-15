BitSetArray
===========

This package contains a simple high performance mutable BitSet implementation for Haskell.

It is based around the notion of having a (mutable) array of word64's and
achieving performance by **removing bounds checking**. As such it performs well
but can be dangerous to use if you provide an out of bounds insertion or removal.

The functions provided are mainly those required for performing bit encoded
maximum clique colouring as in
[An exact bit-parallel algorithm for the maximum clique problem - San Segundo et al.](http://www.sciencedirect.com/science/article/pii/S0305054810001504
"Link to Paper"), although it could be used for many other tasks.

Future
======

+ Investigate performance optimisations further and test this on a wider range of benchmarks.
+ ST version to avoid forcing the user into IO
