use slab::Slab;
use std::{borrow::Borrow, cmp::Ordering, collections::{BTreeSet, BinaryHeap}, ops::Bound};

use crate::{
    crossable::Crossable,
    events::{Event, EventType, SweepPoint},
    line_or_point::LineOrPoint,
};

/// A segment of input [`LineOrPoint`] generated during the sweep.
#[derive(Debug)]
pub struct Segment<'a, C: Crossable> {
    geom: LineOrPoint<C::Scalar>,
    key: usize,
    crossable: &'a C,
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
    pub(crate) fn new(
        storage: &mut Slab<Self>,
        crossable: &'a C,
        geom: Option<LineOrPoint<C::Scalar>>,
    ) -> Self {
        let geom = geom.unwrap_or_else(|| crossable.geom().0);
        let entry = storage.vacant_entry();

        let segment = Segment {
            key: entry.key(),
            crossable,
            geom,
        };
        entry.insert(segment);
        segment
    }

    pub(crate) fn push_events(&self, events: &mut BinaryHeap<Event<C::Scalar>>) {
        match self.geom {
            LineOrPoint::Point(p) => {
                events.push(Event {
                    point: p,
                    ty: EventType::PointLeft,
                    segment_key: self.key,
                });
                events.push(Event {
                    point: p,
                    ty: EventType::PointRight,
                    segment_key: self.key,
                });
            }
            LineOrPoint::Line(p, q) => {
                events.push(Event {
                    point: p,
                    ty: EventType::LineLeft,
                    segment_key: self.key,
                });
                events.push(Event {
                    point: q,
                    ty: EventType::LineRight,
                    segment_key: self.key,
                });
            }
        }
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
    fn get_prev_key(&self, segment: &Self::SegmentType, storage: &Slab<Self::SegmentType>)
        -> Option<usize>;
    fn get_next_key(&self, segment: &Self::SegmentType, storage: &Slab<Self::SegmentType>)
        -> Option<usize>;
}

impl<'a, C: Crossable + 'a> AdjacentSegments for BTreeSet<ActiveSegment<'a, C>> {
    type SegmentType = Segment<'a, C>;

    fn get_prev_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        let aseg = ActiveSegment::new(segment.key, storage);
        self.range((Bound::Unbounded, Bound::Excluded(aseg)))
            .next_back()
            .map(|s| s.key)
    }

    fn get_next_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        let aseg = ActiveSegment::new(segment.key, storage);
        self.range((Bound::Excluded(aseg), Bound::Unbounded))
            .next()
            .map(|s| s.key)
    }
}
