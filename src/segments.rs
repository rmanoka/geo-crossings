use geo::GeoFloat;
use slab::Slab;
use std::{cmp::Ordering, collections::BTreeSet, ops::Bound};

use crate::{
    crossable::Crossable,
    events::{Event, EventType},
    line_or_point::LineOrPoint::{self, *},
};

/// A segment of input [`LineOrPoint`] generated during the sweep.
#[derive(Debug)]
pub struct Segment<'a, C: Crossable> {
    pub(crate) geom: LineOrPoint<C::Scalar>,
    key: usize,
    crossable: &'a C,
    pub(crate) overlapping: Option<usize>,
}

// Manual implementation to not require `C: Clone`, same as `Copy`.
impl<'a, C: Crossable> Clone for Segment<'a, C> {
    fn clone(&self) -> Self {
        *self
    }
}
// Manual implementation to not require `C: Copy`.
impl<'a, C: Crossable> Copy for Segment<'a, C> {}

impl<'a, C: Crossable> Segment<'a, C> {
    /// Create and store a `Segment` with given `crossable`, and optional `geom`
    /// (or the default geom).
    pub(crate) fn new<'b>(
        storage: &'b mut Slab<Self>,
        crossable: &'a C,
        geom: Option<LineOrPoint<C::Scalar>>,
    ) -> &'b mut Self {
        let geom = geom.unwrap_or_else(|| crossable.geom().0);
        let entry = storage.vacant_entry();

        let segment = Segment {
            key: entry.key(),
            crossable,
            geom,
            overlapping: None,
        };
        entry.insert(segment)
    }

    /// Get an event for the left end-point (start) of this segment.
    pub(crate) fn left_event(&self) -> Event<C::Scalar> {
        match self.geom {
            Point(p) => Event {
                point: p,
                ty: EventType::PointLeft,
                segment_key: self.key,
            },
            Line(p, _) => Event {
                point: p,
                ty: EventType::LineLeft,
                segment_key: self.key,
            },
        }
    }

    /// Get an event for the right end-point (end) of this segment.
    pub(crate) fn right_event(&self) -> Event<C::Scalar> {
        match self.geom {
            Point(p) => Event {
                point: p,
                ty: EventType::PointRight,
                segment_key: self.key,
            },
            Line(_, q) => Event {
                point: q,
                ty: EventType::LineRight,
                segment_key: self.key,
            },
        }
    }

    /// Get events for both the end-points of this segment.
    pub(crate) fn events(&self) -> [Event<C::Scalar>; 2] {
        [self.left_event(), self.right_event()]
    }

    /// Split a line segment into pieces at points of intersection.
    ///
    /// The initial segment is mutated in place such that ordering
    /// among `ActiveSegment`s does not change. The extra geometries
    /// are returned.
    pub(crate) fn adjust_for_intersection(
        &mut self,
        intersection: LineOrPoint<C::Scalar>,
    ) -> SplitSegments<C::Scalar> {
        use SplitSegments::*;

        // We only support splitting a line segment.
        let (p, q) = match self.geom {
            Point(_) => panic!("attempt to adjust a point segment"),
            Line(p, q) => (p, q),
        };

        match intersection {
            // Handle point intersection
            Point(r) => {
                if p == r || q == r {
                    // If the intersection is at the end point, the
                    // segment doesn't need to be split.
                    Unchanged { overlap: false }
                } else {
                    // Otherwise, split it. Mutate `self` to be the
                    // first part, and return the second part.
                    self.geom = Line(p, r);
                    SplitOnce {
                        overlap: None,
                        right: Line(r, q),
                    }
                }
            }
            // Handle overlapping segments
            Line(r1, r2) => {
                if p == r1 {
                    if r2 == q {
                        // The whole segment overlaps.
                        Unchanged { overlap: true }
                    } else {
                        self.geom = Line(p, r2);
                        SplitOnce {
                            overlap: Some(false),
                            right: Line(r2, q),
                        }
                    }
                } else if r2 == q {
                    self.geom = Line(p, r1);
                    SplitOnce {
                        overlap: Some(true),
                        right: Line(r2, q),
                    }
                } else {
                    self.geom = Line(p, r1);
                    SplitTwice { right: Line(r2, q) }
                }
            }
        }
    }

    /// Get a reference to the segment's crossable.
    pub fn crossable(&self) -> &'a C {
        self.crossable
    }

    /// Get the segment's key.
    pub(crate) fn key(&self) -> usize {
        self.key
    }
}

/// Internal representation used in ordered sets.
pub(crate) struct ActiveSegment<'a, C: Crossable> {
    key: usize,
    storage: *const Slab<Segment<'a, C>>,
}

impl<'a, C: Crossable> ActiveSegment<'a, C> {
    fn new(key: usize, storage: &Slab<Segment<'a, C>>) -> Self {
        ActiveSegment {
            key,
            storage: storage as *const _,
        }
    }
    fn get(&self) -> Option<&Segment<'a, C>> {
        let slab = unsafe { &*self.storage as &Slab<Segment<_>> };
        slab.get(self.key)
    }
}

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<'a, C: Crossable> PartialEq for ActiveSegment<'a, C> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Assert total equality.
impl<'a, C: Crossable> Eq for ActiveSegment<'a, C> {}

/// Partial ordering defined as per algorithm.
///
/// This is requires the same pre-conditions as for [`LineOrPoint`].
impl<'a, C: Crossable> PartialOrd for ActiveSegment<'a, C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get()
            .expect("ActiveSegment::partial_cmp: could not find key in storage")
            .geom
            .partial_cmp(
                &other
                    .get()
                    .expect("ActiveSegment::partial_cmp: could not find key in storage")
                    .geom,
            )
            .map(|o| o.then_with(|| self.key.cmp(&other.key)))
    }
}

/// Assert total ordering same as `PartialOrd` impl.
impl<'a, C: Crossable> Ord for ActiveSegment<'a, C> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

/// Helper trait to get adjacent segments from ordered set.
pub(crate) trait AdjacentSegments {
    type SegmentType;
    fn prev_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize>;
    fn next_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize>;
    fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
}

impl<'a, C: Crossable + 'a> AdjacentSegments for BTreeSet<ActiveSegment<'a, C>> {
    type SegmentType = Segment<'a, C>;

    fn prev_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        let aseg = ActiveSegment::new(segment.key, storage);
        self.range((Bound::Unbounded, Bound::Excluded(aseg)))
            .next_back()
            .map(|s| s.key)
    }

    fn next_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        let aseg = ActiveSegment::new(segment.key, storage);
        self.range((Bound::Excluded(aseg), Bound::Unbounded))
            .next()
            .map(|s| s.key)
    }

    fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        assert!(storage.contains(key));
        assert!(self.insert(ActiveSegment::new(key, storage)));
    }

    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        assert!(storage.contains(key));
        assert!(self.remove(&ActiveSegment::new(key, storage)));
    }
}

pub(crate) enum SplitSegments<T: GeoFloat> {
    Unchanged {
        overlap: bool,
    },
    SplitOnce {
        overlap: Option<bool>,
        right: LineOrPoint<T>,
    },
    SplitTwice {
        right: LineOrPoint<T>,
    },
}
