# Line Crossings

Identifying all intersections of a collection of line-segments is
an important sub-routine in many comp. geom. algorithms. The
simplest implementation of this checks all pairs of segments and
takes $O(n^2)$ time. This problem has been studied extensively,
and faster algorithms are known.

## [Bentley-Ottman] Algorithm

The Bentley-Ottman algorithm is a classical algorithm that lists
all intersections in time $O((n+k) \log n)$, where $k$ is the
number of intersecting (or overlapping) pairs. This is faster
than checking all-pairs for the typical case, where $k \ll
O(n^2).$ It is also a practical algorithm, and appears in some
variation in many geom. libraries.

The algorithm sweeps a vertical (infinite) line from left ($x =
-\infty$) to right ($x = \infty$), while keeping track of
segments that intersect the line at any specific instant ordered
by the $y$ coordinate of its intersection with the sweep line.
The key observations are that:

1. any two intersecting segments were next to each other just
   before the sweep line touches the intersecting point
1. the set of segments that intersect the sweep line, and their
   relative ordering change only at discrete points: either the
   end points of the segments, or their intersections

The first observation allows us to enumerate all intersections of
a segment by only checking the previous and next segment along
the sweep line. The second observation allows us to efficiently
simulate the sweep as a discrete set of $n + k$ events where the
tracked segments or their orderings change.

## Our Implementation

We implement
Our implementation here is essentially the same


## Handling Degeneracy

When breaking segments, mark segments that are
continuations. Record intersections by collecting all heap
pops at the same point.

TODO: Work out handling of overlapping segments.

TODO: Explain additional tie-breaks in ordering for the
Martinez-Rueda and related algorithms.

## Handling Inexact Constructions

For inexact computations (eg. `f64`), we must ensure that
when we compute the intersection, the smaller segments are
still ordered consistently. This is not possible in general
though. We could detect inconsistencies and error out
suggesting that higher precision data-type is to be used.


[Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
