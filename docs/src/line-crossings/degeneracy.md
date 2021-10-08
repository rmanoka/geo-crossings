# General Case

Having explained the working of the algorithm, we now generalize
by removing the assumptions we had earlier.

## Intersections at end-points

To handle intersections at the end-points, we scan the sweep
points as encountered and group them into consecutive groups of
events that were triggerred at the same coordinate. Each event is
associated with the segment and the end-point (left or right)
that triggerred it. Further, we also keep track of the original
input segment whenever we split it segments.

This collation gives us a sequence of distinct points, where each
point is associated with a sequence of segments. Some of these
segments may have come from the same input segment, but the set
of input segments of the sequence (discounting repetitions) are
all the intersections at the point of interest. This also unifies
handling of interior intersections as the segments are split at
these interior points.

See `CrossingsIter` for the user-facing iterator which yields a
point, and allows to query the set of segments touching this
point. The `Crossing` structure captures each segment, which
in-turn contains a reference to the user-input: a type `T:
Crossable`.

## Multiple intersections

The above, and the splitting logic also handles multiple
intersections. When the first intersection is witnessed, the two
segments are split, after which each sub-sequent segment that
intersects one of these (new) end-points is split at that point.
This ensures that all the intermediate end-points occurs at the
same coordinate and are grouped together.

## Handling Overlaps

Overlaps are handled by again splitting the segment into parts
which either fully overlap or do not overlap at all. Then only
one of the fully overlapping segment is processed, and the rest
are stored as a linked-list in the one to be processed. Then, for
all further reporting or splitting, any process applied to the
root segment is propogated through the chain of overlapping
segments.

The user receives two events corresponding to the start and end
of each overlap. The `has_overlap` field of each `Crossing`
denotes if the next segment (another `Crossing`) in the slice
returned by `CrossingsIter::intersections` method overlaps with
it.

## Handling Point-Line intersection

While the description of the algorithm only considers
line-segments, we also accept input geoms to represent points.
These are represented internally as infinitesimally small
vertical segments centered at the point. This is independent of
the scalar accuracy (`f32` or `f64`), and is handled by tweaking
the ordering of segments to represent these. A user provides a
point by associating a `Line` with the same `start` and `end`
coordinates.

## Numerical Precision

TODO
