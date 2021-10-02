use geo::{Coordinate, GeoFloat};
use std::cmp::Ordering;

/// A sweep event for sweep-line algorithms.
#[derive(Debug, Clone)]
pub(crate) struct Event<T: GeoFloat> {
    pub point: SweepPoint<T>,
    pub ty: EventType,
    pub segment_key: usize,
}

/// Equality check for usage in ordered sets. Note that it ignores
/// segment_key.
impl<T: GeoFloat> PartialEq for Event<T> {
    fn eq(&self, other: &Self) -> bool {
        self.point == other.point && self.ty == other.ty
    }
}

/// Assert total equality
impl<T: GeoFloat> Eq for Event<T> {}

/// Ordering for use with a max-heap (`BinaryHeap`). Note that it
/// ignores the segment_key. This suffices for heap usage, where
/// repeated items are allowed.
impl<T: GeoFloat> PartialOrd for Event<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Derive `Ord` from `PartialOrd` and expect to not fail.
impl<T: GeoFloat> Ord for Event<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.point
            .cmp(&other.point)
            .then_with(|| self.ty.cmp(&other.ty))
            .reverse()
    }
}

/// Event type to associate with event.
///
/// The ordering of the variants is important for the algorithm. We
/// require the right end points to be ordered before the left end
/// points in the sweep to support a consistent total ordering of the
/// "active segments". The point variants handle edge case involving
/// point-line intersections.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub(crate) enum EventType {
    PointLeft,
    LineRight,
    LineLeft,
    PointRight,
}

/// Wraps a [`Coordinate`] to support lexicographic ordering.
///
/// The ordering is by `x` and then by `y`. Implements `PartialOrd`,
/// `Ord` and `Eq` even though `Coordinate` doesn't implement these.
/// This is necessary to support insertion to ordered collections,
/// especially `BinaryHeap` as required by sweep algorithms.
///
/// Note that the trait impls exist even when `T` is not `Eq` or
/// `Ord`. We must ensure that any sweep point only contains values
/// that can be consistently ordered.
#[derive(PartialEq, Clone, Copy)]
pub struct SweepPoint<T: GeoFloat>(pub Coordinate<T>);

impl<T: GeoFloat> std::fmt::Debug for SweepPoint<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Pt").field(&self.0.x).field(&self.0.y).finish()
    }
}

/// Implememnt lexicographic ordering by `x` and then by `y`
/// coordinate.
impl<T: GeoFloat> PartialOrd for SweepPoint<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.0.x.partial_cmp(&other.0.x) {
            Some(Ordering::Equal) => self.0.y.partial_cmp(&other.0.y),
            o => o,
        }
    }
}

/// Derive `Ord` from `PartialOrd` and expect to not fail.
impl<T: GeoFloat> Ord for SweepPoint<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

/// We derive `Eq` manually to not require `T: Eq`.
impl<T: GeoFloat> Eq for SweepPoint<T> {}

/// Create from `Coordinate` while checking the components are finite.
impl<T: GeoFloat> From<Coordinate<T>> for SweepPoint<T> {
    fn from(pt: Coordinate<T>) -> Self {
        assert!(
            pt.x.is_finite(),
            "sweep point requires a finite x-coordinate"
        );
        assert!(
            pt.y.is_finite(),
            "sweep point requires a finite y-coordinate"
        );
        SweepPoint(pt)
    }
}
impl<T: GeoFloat> From<(T, T)> for SweepPoint<T> {
    fn from(coords: (T, T)) -> Self {
        Coordinate::from(coords).into()
    }
}

#[cfg(test)]
mod tests {
    use std::iter::from_fn;

    use super::*;

    #[test]
    fn test_sweep_point_ordering() {
        let p1 = SweepPoint::from(Coordinate { x: 0., y: 0. });
        let p2 = SweepPoint::from(Coordinate { x: 1., y: 0. });
        let p3 = SweepPoint::from(Coordinate { x: 1., y: 1. });
        let p4 = SweepPoint::from(Coordinate { x: 1., y: 1. });

        assert!(p1 < p2);
        assert!(p1 < p3);
        assert!(p2 < p3);
        assert!(p3 <= p4);
    }

    #[test]
    fn test_event_ordering() {
        let e1 = Event {
            point: SweepPoint::from(Coordinate { x: 0., y: 0. }),
            ty: EventType::LineLeft,
            segment_key: 0,
        };
        let e2 = Event {
            point: SweepPoint::from(Coordinate { x: 1., y: 0. }),
            ty: EventType::LineRight,
            segment_key: 1,
        };
        let e3 = Event {
            point: SweepPoint::from(Coordinate { x: 1., y: 0. }),
            ty: EventType::LineLeft,
            segment_key: 2,
        };
        let e4 = Event {
            point: SweepPoint::from(Coordinate { x: 1., y: 1. }),
            ty: EventType::LineRight,
            segment_key: 3,
        };

        use std::collections::BinaryHeap;
        let mut heap = BinaryHeap::new();
        heap.push(e1);
        heap.push(e2);
        heap.push(e3);
        heap.push(e4);

        let order: Vec<_> = from_fn(|| heap.pop()).map(|e| e.segment_key).collect();
        assert_eq!(order, vec![0, 1, 2, 3]);
    }
}
