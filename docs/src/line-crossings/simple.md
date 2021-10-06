# [Bentley-Ottman] - Simple Version

The details of the data-structures, and the algorithm are easier
to explain under the following simplifying assumption. We will
see how to lift these assumptions later.

1. Only identify intersections in the interior of segments.
1. No three input segments intersect at the same point.
1. Segments intersect only at a point (i.e., no overlap).
1. All computations of intersections are exact.

## Sweep Iteration

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

The heap is initialized with the left and right end points of all
input segments. The intersection points are added as and when
they are witnessed. The algorithm ensures that every intersection
point is witnessed.


## Ordering the Segments

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

## Tracking Active Segments

The above ordering generalizes to any set of segments $(p_i,
q_i)$ satisfying $p_i < q_j$ for all $i, j.$ The ordering is
consistent with any sweep-line upto (and including) the first
$r_{ij}$ computed for each pair $i, j.$

To track the active segments, we insert a segment into the set
when we encounter (during the sweep) the left end point, and
remove it at the right end point. The lexicographic ordering of
points described above ensures that the active segments satisfy:
$p_i \le q_j$ for any pair $i, j$ of active segments. To make the
inequality strict, we also ensure that the heap orders any right
end-point $q_j$ before all coinciding left end-points $p_i =
q_j.$

### Handling intersections in segment

Since the ordering of segments is incorrect past the first
interior intersection, we ensure that no active segment stays
past this.  This is handled as follows.

When an interior intersection $r$ of a $(p, q)$ is witnessed, we
split the segment into $(p, r)$ and $(r, q)$. The segment $(p,
q)$ may already be active, in which case, it is replaced in-place
with $(p, r)$. Note that this doesn't violate the ordering
established above. Then we add a new right-end point event at
$r$, and also the end-points of the other segment $(r, q).$

Note that ideally we should adjust the priority of the existing
event at $q$ to now trigger at $r$, but this involves too much
book-keeping. Instead, we allow a spurious extra event at $q$
which is then ignored as it doesn't correspond to a valid
segment.

### Adding an Active Segment

When the sweep hits the end point $p$ of a segment $s = (p, q),$
we use the ordered active segments to find (at most) two adjacent
segments $s_i = (p_i, q_i)$ between which it will be inserted.
This query is valid as all the right end points $q_j$ (and $q$
itself) are strictly larger than $p,$ and the current sweep line
is at $p$ which is before any future intersection point.

For each adjacent segment, we check if it intersects the current
segment. If $r$ is such an (interior) intersection, then we
adjust both the segments $s_i$ and $s$ as described in the
previous section. Note that our assumption that no 3 segments
intersect at the same point ensures that each adjustment still
only captures one intersection, and the rest of the intersections
is still witnessed separately.

### Remove an Active Segment

When the sweep reaches the right end-point $q$, we again identify
the adjacent segments $s_1$ and $s_2.$ Then we handle the
intersection of $s_1$ and $s_2$ similar to handling intersection
during insertion.

## Correctness

To see that we witness every interior intersection, consider two
segments $(p_1, q_1)$ and $(p_2, q_2)$ that intersect at an
interior point $r.$ Since no other segment passes through $r$,
there is a point strictly before $r$ where both segments are
active and adjacent in the ordering.

Further, when the first of these two were tracked, they were not
adjacent (as the other was not active yet). This implies there
was a unique first sweep event at which these two became active.
This was either a removal of a segment between these two, or
insertion of one of these. The processing of either of these
witnessed this intersection.

[Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
