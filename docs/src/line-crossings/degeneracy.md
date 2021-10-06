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
