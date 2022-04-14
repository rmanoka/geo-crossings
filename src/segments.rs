use geo::GeoFloat;
use slab::Slab;
use std::{cmp::Ordering, collections::BTreeSet, fmt::Debug, ops::Bound};

use crate::{
    events::{Event, EventType},
    line_or_point::LineOrPoint::{self, *},
    Crossable, Crossing,
};

/// A segment of input [`LineOrPoint`] generated during the sweep.
#[derive(Debug, Clone, Copy)]
pub struct Segment<C: Crossable> {
    pub(crate) geom: LineOrPoint<C::Scalar>,
    key: usize,
    crossable: C,
    first_segment: bool,
    pub(crate) overlapping: Option<usize>,
    pub(crate) is_overlapping: bool,
}

impl<C: Crossable> Segment<C> {
    /// Create and store a `Segment` with given `crossable`, and optional `geom`
    /// (or the default geom).
    pub(crate) fn new(
        storage: &mut Slab<Self>,
        crossable: C,
        geom: Option<LineOrPoint<C::Scalar>>,
    ) -> &mut Self {
        let first = geom.is_none();
        let geom = geom.unwrap_or_else(|| crossable.line().into());
        let entry = storage.vacant_entry();

        let segment = Segment {
            key: entry.key(),
            crossable,
            geom,
            first_segment: first,
            overlapping: None,
            is_overlapping: false,
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
                assert!(
                    p <= r && r <= q,
                    "intersection point was not ordered within the line!"
                );
                if p == r || q == r {
                    // If the intersection is at the end point, the
                    // segment doesn't need to be split.
                    Unchanged { overlap: false }
                } else {
                    // Otherwise, split it. Mutate `self` to be the
                    // first part, and return the second part.
                    self.geom = Line(p, r);
                    self.first_segment = false;
                    SplitOnce {
                        overlap: None,
                        right: Line(r, q),
                    }
                }
            }
            // Handle overlapping segments
            Line(r1, r2) => {
                assert!(
                    p <= r1 && r2 <= q,
                    "overlapping segment was not ordered within the line!"
                );
                if p == r1 {
                    if r2 == q {
                        // The whole segment overlaps.
                        Unchanged { overlap: true }
                    } else {
                        self.geom = Line(p, r2);
                        self.first_segment = false;
                        SplitOnce {
                            overlap: Some(false),
                            right: Line(r2, q),
                        }
                    }
                } else if r2 == q {
                    self.geom = Line(p, r1);
                    self.first_segment = false;
                    SplitOnce {
                        overlap: Some(true),
                        right: Line(r1, q),
                    }
                } else {
                    self.geom = Line(p, r1);
                    self.first_segment = false;
                    SplitTwice { right: Line(r2, q) }
                }
            }
        }
    }

    /// Get a reference to the segment's crossable.
    pub fn crossable(&self) -> &C {
        &self.crossable
    }

    /// Get the segment's key.
    pub(crate) fn key(&self) -> usize {
        self.key
    }

    /// Convert `self` into a `Crossing` to return to user.
    pub(crate) fn into_crossing(self, event_ty: EventType) -> Crossing<C> {
        Crossing {
            crossable: self.crossable,
            line: self.geom.line(),
            first_segment: self.first_segment,
            has_overlap: self.overlapping.is_some(),
            at_left: event_ty == EventType::LineLeft,
        }
    }
}

/// Internal representation used in ordered sets.
pub(crate) struct ActiveSegment<C: Crossable> {
    key: usize,
    storage: *const Slab<Segment<C>>,
}

impl<C: Crossable> Debug for ActiveSegment<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveSegment")
            .field("key", &self.key)
            .field("segment", &self.get())
            .finish()
    }
}

impl<C: Crossable> ActiveSegment<C> {
    /// Create a new active segment pointing to the given storage.
    ///
    /// # Safety
    ///
    /// This function is unsafe. Caller must ensure that the `storage`
    /// reference is valid and constant during the lifetime of the
    /// created object. Further, the segment at `key` must remain in
    /// place, and not be deleted; it may be modified as long as it's
    /// ordering with respect to other active segments is preserved
    /// (required by the `PartialOrd` impl.).
    unsafe fn new(key: usize, storage: &Slab<Segment<C>>) -> Self {
        ActiveSegment {
            key,
            storage: storage as *const _,
        }
    }
    fn get(&self) -> Option<&Segment<C>> {
        // Safety: reference is guaranteed to be valid by the `new`
        // method.
        let slab = unsafe { &*self.storage as &Slab<Segment<_>> };
        slab.get(self.key)
    }
}

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<C: Crossable> PartialEq for ActiveSegment<C> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Assert total equality.
impl<C: Crossable> Eq for ActiveSegment<C> {}

/// Partial ordering defined as per algorithm.
///
/// This is requires the same pre-conditions as for [`LineOrPoint`].
impl<C: Crossable> PartialOrd for ActiveSegment<C> {
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
impl<C: Crossable> Ord for ActiveSegment<C> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .expect("unable to compare active segments!")
    }
}

/// Helper trait to insert, remove and get adjacent segments from ordered set.
pub(crate) trait SegmentAccess {
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
    unsafe fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
}

impl<C: Crossable> SegmentAccess for BTreeSet<ActiveSegment<C>> {
    type SegmentType = Segment<C>;

    #[inline]
    fn prev_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding an immut. reference to the storage.
        let aseg = unsafe { ActiveSegment::new(segment.key, storage) };
        self.range((Bound::Unbounded, Bound::Excluded(aseg)))
            .next_back()
            .map(|s| s.key)
    }

    #[inline]
    fn next_key(
        &self,
        segment: &Self::SegmentType,
        storage: &Slab<Self::SegmentType>,
    ) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding a immut. reference to the storage.
        let aseg = unsafe { ActiveSegment::new(segment.key, storage) };
        self.range((Bound::Excluded(aseg), Bound::Unbounded))
            .next()
            .map(|s| s.key)
    }

    #[inline]
    unsafe fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        assert!(storage.contains(key));
        assert!(self.insert(ActiveSegment::new(key, storage)));
    }

    #[inline]
    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        assert!(storage.contains(key));
        // Safety: temporary active segment is valid as we're holding
        // a immut. reference to `storage`.
        assert!(self.remove(&unsafe { ActiveSegment::new(key, storage) }));
    }
}

/// Stores the type of split and extra geometries from adjusting a
/// segment for intersection.
#[derive(Debug)]
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

#[cfg(test)]
mod tests {
    use geo::Line;

    use super::*;

    impl<T: GeoFloat> PartialEq for SplitSegments<T> {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (
                    Self::Unchanged { overlap: l_overlap },
                    Self::Unchanged { overlap: r_overlap },
                ) => l_overlap == r_overlap,
                (
                    Self::SplitOnce {
                        overlap: l_overlap,
                        right: l_right,
                    },
                    Self::SplitOnce {
                        overlap: r_overlap,
                        right: r_right,
                    },
                ) => l_overlap == r_overlap && l_right.coords_equal(r_right),
                (Self::SplitTwice { right: l_right }, Self::SplitTwice { right: r_right }) => {
                    l_right.coords_equal(r_right)
                }
                _ => false,
            }
        }
    }

    #[test]
    fn test_split() {
        let mut slab = Slab::new();
        let lines = vec![
            Line::from([(0., 0.), (10., 10.)]),
            [(10.0, 0.), (0., 10.)].into(),
            [(0., 0.), (0., 10.)].into(),
            [(0., 0.), (5., 5.)].into(),
            [(10., 10.), (5., 5.)].into(),
        ];
        lines.into_iter().enumerate().for_each(|(i, l)| {
            assert_eq!(Segment::new(&mut slab, l, None).key(), i);
        });

        struct TestCase {
            a: usize,
            b: usize,
            isec: Option<LineOrPoint<f64>>,
            split: Option<SplitSegments<f64>>,
        }

        impl TestCase {
            fn assert_equality(&self, slab: &Slab<Segment<Line<f64>>>) {
                let isec = slab[self.a].geom.intersect_line(&slab[self.b].geom);
                assert_eq!(isec, self.isec);

                if isec.is_none() {
                    return;
                }
                let isec = isec.unwrap();
                let mut copy_seg = slab[self.a];
                let split = copy_seg.adjust_for_intersection(isec);
                assert_eq!(&split, self.split.as_ref().unwrap(),)
            }
        }

        let test_cases = vec![
            TestCase {
                a: 0,
                b: 0,
                isec: Some(slab[0].geom.clone()),
                split: Some(SplitSegments::Unchanged { overlap: true }),
            },
            TestCase {
                a: 0,
                b: 1,
                isec: Some(LineOrPoint::Point((5., 5.).into())),
                split: Some(SplitSegments::SplitOnce {
                    overlap: None,
                    right: LineOrPoint::Line((5., 5.).into(), (10., 10.).into()),
                }),
            },
            TestCase {
                a: 0,
                b: 2,
                isec: Some(LineOrPoint::Point((0., 0.).into())),
                split: Some(SplitSegments::Unchanged { overlap: false }),
            },
            TestCase {
                a: 0,
                b: 3,
                isec: Some(LineOrPoint::Line((0., 0.).into(), (5., 5.).into())),
                split: Some(SplitSegments::SplitOnce {
                    overlap: Some(false),
                    right: LineOrPoint::Line((5., 5.).into(), (10., 10.).into()),
                }),
            },
            TestCase {
                a: 0,
                b: 4,
                isec: Some(LineOrPoint::Line((5., 5.).into(), (10., 10.).into())),
                split: Some(SplitSegments::SplitOnce {
                    overlap: Some(true),
                    right: LineOrPoint::Line((5., 5.).into(), (10., 10.).into()),
                }),
            },
        ];

        test_cases.iter().for_each(|t| t.assert_equality(&slab));
    }
}
