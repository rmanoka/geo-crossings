# Line Crossings

Identifying all intersections of a collection of line-segments is
an important sub-routine in many comp. geom. algorithms. The
simplest implementation of this &mdash; checking all pairs of
segments &mdash; takes $O(n^2)$ time. This problem has been
studied extensively, and faster algorithms are known.

## [Bentley-Ottman] Algorithm

The Bentley-Ottman algorithm is a classical algorithm that lists
all intersections in time $O((n+k) \log(n))$, where $k$ is the
number of intersecting (or overlapping) pairs. This is faster
than checking all-pairs for the typical case: $k \ll O(n^2).$ It
is also a practical algorithm, and appears in some variation in
many geom. libraries.

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

The details of the data-structures, and the algorithm are easier
to explain under the following simplifying assumption. We will
see how to lift these assumptions later.

1. Only identify intersections in the interior of segments.
1. No three input segments intersect at the same point.
1. All computations of intersections are exact.

### Sweep Iteration

The sweep is simulated as an iteration of events corresponding to
the sweep-line reaching the end points, or intersection points of
the segments. As the intersection points are discovered as the
sweep proceeds, we use a min-heap over the events, ordered by the
point it corresponds to iterate over all the events.

Event points are ordered by the x-coordinate to simulate the
sweep, and then by the y-coordinate to break ties. The ordering
by y-coordinate ensures a fixed consistent ordering of the end
points of a vertical segment. Henceforth, we denote a segment as
the tuple of the left and right end points $(p, q)$ where $p < q$
as per the above ordering. Note that if $p = q$, then it does not
have an interior, and thus ignored under our assumptions.

If two events are at the same point, the right end points are
ordered before left end points. This is needed to define a
consistent ordering of segments associated with the current
sweep-line.


### Ordering the Segments

To query segments above and below a given segment along the
sweep-line, we track all segments intersecting the current
sweep-line (a.k.a _active segments_) as an ordered set. However,
this ordering is not fixed and depends on the position of the
sweep-line which violates the requirements of ordered set
data-structures such as `BTreeSet`.

We address this by identifying pre-condition under which a
collection of segments have a well-defined total ordering
consistent with a given sweep-line. Then, we ensure that this
condition is always satisfied by the set of active segments.

For two segments, $(p_1, q_1)$, and $(p_2, q_2)$, we define the
ordering as follows. Assume $p_1 \le p_2$ as otherwise, we can
swap them. We also require that $p_i < q_j$ for $i, j \in \{1,
2\}$. We use the orientation predicate that indicates if three
points $x, y, z$ are oriented clockwise, counter-clockwise or
collinear.

```rust
fn cmp_segments(s1: Segment, s2: Segment) -> Ordering {
    let (p1, q1) = s1;
    let (p2, q2) = s2;
    // pre-condition: p1 <= p2
    // pre-condition: p2 < q1

    fn orientation_as_ordering(o: Orientation) -> Ordering {
        match o {
            CounterClockwise => Less,
            Clockwise => Greater,
            Collinear => Equal,
        }
    }

    orientation_as_ordering(orient2d(p1, q1, p2))
        .then_with(|| {
            // p1, q1, p2 are collinear
            // use q2 to define the ordering
            orientation_as_ordering(orient2d(p1, q1, q2)
        })
        .then_with(|| {
            // arbitrary but a fixed tie-breaker
        })
}
```

It is not hard to see that for non-intersecting segments, this is
in fact consistent with any common sweep-line. If the segments
intersect, then let $r$ be the first intersection in the interior
of both segments or the first right end-point if there is no such
point. Then, the above ordering is consistent with any common
sweep-line upto one passing through $r$.

Note that collinear segments (and even segments with the same end
points) must still be ordered. This may be arbitrary, but fixed
ordering such as by their index in an array, or a unique id.

This can be generalized to any set of segments $(p_i, q_i)$
satisfying $p_i < q_j$ for all $i, j.$ The ordering is consistent
with any sweep-line upto the first $r_{ij}$ computed for each
pair $i, j.$

### Tracking Active Segments

To track the active segments, we insert a segment into the
set when we encounter the left end point, and remove it at
the right end point. The lexicographic ordering of points
described above ensures that the active segments satisfy:
$p_i \le q_j$ for any pair $i, j$ of active segments. To
make the inequality strict, we must also ensure that the
heap orders any right end-point $q_j$ before all coinciding
left end-points $p_i = q_j.$

As described in the outline, we look for intersections
before inserting a new segment, or after removing a segment.
Since interior intersections are not allowed among the
active segments, we break the segment with interior
intersection.  There are two cases to consider here.

1. Intersection at a point: in this case, we break the
   segments into exactly 4 segments as the point is in the
   interior. At most two of these segments are already
   active whose end point must be shortened to the
   intersection point.
1. Overlapping segments: in this case we break it into one
   overlapping segment, and 2 additional segments. One of
   these is already active and must be shortened.

In either case, the current point $x$ is strictly smaller
than the intersection or the first overlap point. This is
because the intersections are in the interior of the
segments. Thus, any existing existing segments can be
shortened to the intersections without violating the
constraint on the end points. It also does not affect the
ordering of the active segments (TODO: proof).

### Correctness

Prove that each intersection is visited, and exactly once.

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
