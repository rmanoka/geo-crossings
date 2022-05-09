use geo::GeoFloat;
use std::{cmp::Ordering, collections::BTreeSet, fmt::Debug};

use crate::{
    events::{Event, EventType, SweepPoint},
    line_or_point::LineOrPoint::{self, *},
    segments::{ActiveSegment, SegmentAccess},
    Crossable, Crossing,
};
use std::collections::BinaryHeap;

use log::{debug, trace};
use slab::Slab;

/// A segment of input [`LineOrPoint`] generated during the sweep.
#[derive(Debug, Clone, Copy)]
struct Segment<C: Crossable> {
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
                debug_assert!(
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
                debug_assert!(
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

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<C: Crossable> PartialEq for Segment<C> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Partial ordering defined as per algorithm.
///
/// This is requires the same pre-conditions as for [`LineOrPoint`].
impl<C: Crossable> PartialOrd for Segment<C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.geom
            .partial_cmp(&other.geom)
            .map(|o| o.then_with(|| self.key.cmp(&other.key)))
    }
}

/// Sweep algorithm for detecting all crossings.
///
/// This is an internal data-structure that implements the
/// [Bentley-Ottman] sweep. Maintains a heap of heap of end-points,
/// and the currently active segments. End-users should use one of the
/// iterator interfaces built around this sweep.
///
/// [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
pub struct Sweep<C: Crossable>
where
    C: Crossable + Clone,
{
    segments: Box<Slab<Segment<C>>>,
    events: BinaryHeap<Event<C::Scalar>>,
    active_segments: BTreeSet<ActiveSegment<Segment<C>>>,
}

impl<C: Crossable + Clone> Sweep<C> {
    pub fn new<I: IntoIterator<Item = C>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let size = {
            let (min_size, max_size) = iter.size_hint();
            max_size.unwrap_or(min_size)
        };

        let mut sweep = Sweep {
            segments: Slab::with_capacity(size).into(),
            events: BinaryHeap::with_capacity(size),
            active_segments: Default::default(),
        };
        for cr in iter {
            sweep.create_segment(cr, None, None);
        }

        sweep
    }

    /// Create a segment, and add its events into the heap.
    ///
    /// # Arguments
    ///
    /// - `crossable` - the user input associated with this segment
    /// - `geom` - the geometry (if `None`, takes the default geom from
    /// crossable)
    /// - `parent` - the chain of overlapping segments to copy from (`None` if
    /// no overlap)
    fn create_segment(
        &mut self,
        crossable: C,
        geom: Option<LineOrPoint<C::Scalar>>,
        parent: Option<usize>,
    ) -> usize {
        let segment = Segment::new(&mut self.segments, crossable, geom);

        // Push events to process the created segment.
        for e in segment.events() {
            self.events.push(e);
        }
        let segment_key = segment.key();

        if let Some(parent) = parent {
            let segment_geom = segment.geom;

            let mut child = self.segments[parent].overlapping;
            let mut target_key = segment_key;

            while let Some(child_key) = child {
                let child_overlapping = self.segments[child_key].overlapping;
                let child_crossable = self.segments[child_key].crossable().clone();

                let new_key =
                    Segment::new(&mut self.segments, child_crossable, Some(segment_geom)).key();
                self.segments[target_key].overlapping = Some(new_key);

                target_key = new_key;
                child = child_overlapping;
            }
        }
        segment_key
    }

    /// Adjust the segment at `key` for a detected intersection
    /// (`adj_intersection`).
    ///
    /// This is a wrapper around [`Segment::adjust_for_intersection`]
    /// but also pushes right-end event if the segment geometry
    /// changed. Also handles overlaps by adjusting the geom on all
    /// overlapping segments.
    fn adjust_for_intersection(
        &mut self,
        key: usize,
        adj_intersection: LineOrPoint<C::Scalar>,
    ) -> SplitSegments<C::Scalar> {
        let segment = &mut self.segments[key];
        debug!(
            "adjust_for_intersection: {:?}\n\twith: {:?}",
            segment, adj_intersection
        );
        let adjust_output = segment.adjust_for_intersection(adj_intersection);
        debug!("adjust_output: {:?}", adjust_output);
        let new_geom = segment.geom;

        use SplitSegments::*;
        if matches!(adjust_output, SplitOnce { .. } | SplitTwice { .. }) {
            self.events.push(segment.right_event());
        }

        let mut child = segment.overlapping;
        while let Some(child_key) = child {
            let child_seg = &mut self.segments[child_key];
            child_seg.geom = new_geom;
            child = child_seg.overlapping;
        }
        adjust_output
    }

    /// Adjust the segment at `key` for a detected intersection
    /// (`adj_intersection`). Splits the segment, and adds the extra
    /// segments (copying overlaps appropriately). Returns the key of
    /// the overlapping segment, if any.
    fn adjust_one_segment(
        &mut self,
        key: usize,
        adj_intersection: LineOrPoint<C::Scalar>,
    ) -> Option<usize> {
        let adj_segment = &mut self.segments[key];
        let adj_cross = adj_segment.crossable().clone();
        use SplitSegments::*;
        match self.adjust_for_intersection(key, adj_intersection) {
            Unchanged { overlap } => overlap.then(|| key),
            SplitOnce { overlap, right } => {
                let new_key = self.create_segment(adj_cross, Some(right), Some(key));
                match overlap {
                    Some(false) => Some(key),
                    Some(true) => Some(new_key),
                    None => None,
                }
            }
            SplitTwice { right } => {
                self.create_segment(adj_cross.clone(), Some(right), Some(key));
                let middle_key = self.create_segment(adj_cross, Some(adj_intersection), Some(key));
                Some(middle_key)
            }
        }
    }

    /// Check event is valid, and return the segment associated with it.
    ///
    /// If a segment was adjusted, we may have spurious event for the
    /// right end point (`LineRight`) which is no longer valid.
    /// Returns `None` in this case.
    fn segment_for_event(&self, event: &Event<C::Scalar>) -> Option<&Segment<C>> {
        use EventType::*;
        use LineOrPoint::*;
        Some({
            let maybe_segment = self.segments.get(event.segment_key);
            if let LineRight = event.ty {
                match maybe_segment {
                    Some(segment) if !segment.is_overlapping => match segment.geom {
                        Line(_, q) if q == event.point => segment,
                        _ => return None,
                    },
                    _ => return None,
                }
            } else {
                let segment = maybe_segment.expect("segment for event not found in storage");
                match event.ty {
                    LineLeft => match segment.geom {
                        Line(p, _) => assert_eq!(p, event.point),
                        _ => panic!("unexpected segment type for event"),
                    },
                    PointLeft | PointRight => match segment.geom {
                        Point(p) => assert_eq!(p, event.point),
                        _ => panic!("unexpected segment type for event"),
                    },
                    _ => unreachable!(),
                }
                segment
            }
        })
    }

    /// Chain a new segment at `tgt_key` detected as overlapping with
    /// an existing segment at `src_key`.
    fn chain_overlap(&mut self, src_key: usize, tgt_key: usize) {
        let mut segment = &mut self.segments[src_key];
        while let Some(ovlp_key) = segment.overlapping {
            segment = &mut self.segments[ovlp_key];
        }
        segment.overlapping = Some(tgt_key);
        self.segments[tgt_key].is_overlapping = true;
    }

    /// Handle one event.
    ///
    /// Returns `true` if the event was not spurious.
    fn handle_event<F: FnMut(Crossing<C>)>(&mut self, event: Event<C::Scalar>, cb: &mut F) -> bool {
        use EventType::*;

        trace!("handling event: {:?}", event);
        let mut segment = match self.segment_for_event(&event) {
            Some(s) => s,
            None => return false,
        }
        .clone();

        let prev = self.active_segments.prev_key(segment.key(), &self.segments);
        let next = self.active_segments.next_key(segment.key(), &self.segments);

        match &event.ty {
            LineLeft => {
                for adj_key in prev.into_iter().chain(next.into_iter()) {
                    let adj_segment = self
                        .segments
                        .get_mut(adj_key)
                        .expect("active segment not found in storage");
                    if let Some(adj_intersection) = segment.geom.intersect_line(&adj_segment.geom) {
                        debug!("Found intersection:\n\tsegment1: {:?}\n\tsegment2: {:?}\n\tintersection: {:?}", segment, adj_segment, adj_intersection);
                        // 1. Split adj_segment, and extra splits to storage
                        let adj_overlap_key = self.adjust_one_segment(adj_key, adj_intersection);

                        // A special case is if adj_segment was split, and the
                        // intersection is at the start of this segment. In this
                        // case, there is an right-end event in the heap, that
                        // needs to be handled before finishing up this event.
                        let handle_end_event = {
                            // Get first point of intersection
                            let int_pt = adj_intersection.first();
                            // Check its not first point of the adjusted, but is
                            // first point of current segment
                            int_pt != self.segments[adj_key].geom.first()
                                && int_pt == segment.geom.first()
                        };
                        if handle_end_event {
                            let event = self.events.pop().unwrap();
                            let done = self.handle_event(event, cb);
                            debug_assert!(done, "special right-end event handling failed")
                        }

                        // 2. Split segment, adding extra segments as needed.
                        let seg_overlap_key =
                            self.adjust_one_segment(event.segment_key, adj_intersection);
                        // Update segment as it may have changed in storage
                        segment = self.segments[event.segment_key].clone();

                        assert_eq!(
                            adj_overlap_key.is_some(),
                            seg_overlap_key.is_some(),
                            "one of the intersecting segments had an overlap, but not the other!"
                        );
                        if let Some(adj_ovl_key) = adj_overlap_key {
                            let tgt_key = seg_overlap_key.unwrap();
                            self.chain_overlap(adj_ovl_key, tgt_key);

                            if tgt_key == event.segment_key {
                                // The whole event segment is now overlapping
                                // some other active segment.
                                return true;
                            }
                        }
                    }
                }

                // Add current segment as active

                // Safety: `self.segments` is a `Box` that is not
                // de-allocated until `self` is dropped.
                unsafe {
                    self.active_segments
                        .add_segment(event.segment_key, &self.segments);
                }

                let mut segment_key = Some(event.segment_key);
                while let Some(key) = segment_key {
                    let segment = &self.segments[key];
                    cb(segment.clone().into_crossing(event.ty));
                    segment_key = segment.overlapping;
                }
            }
            LineRight => {
                // Safety: `self.segments` is a `Box` that is not
                // de-allocated until `self` is dropped.
                self.active_segments
                    .remove_segment(event.segment_key, &self.segments);

                let mut segment_key = Some(event.segment_key);
                while let Some(key) = segment_key {
                    let segment = &self.segments[key];
                    cb(segment.clone().into_crossing(event.ty));
                    segment_key = segment.overlapping;
                    self.segments.remove(key);
                }

                if let (Some(prev_key), Some(next_key)) = (prev, next) {
                    let prev_geom = self.segments[prev_key].geom;
                    let next_geom = self.segments[next_key].geom;
                    if let Some(adj_intersection) = prev_geom.intersect_line(&next_geom) {
                        // 1. Split prev_segment, and extra splits to storage
                        let first = self
                            .adjust_one_segment(prev_key, adj_intersection)
                            .is_none();
                        let second = self
                            .adjust_one_segment(next_key, adj_intersection)
                            .is_none();
                        debug_assert!(
                            first && second,
                            "adjacent segments @ removal can't overlap!"
                        );
                    }
                }
            }
            PointLeft => {
                for adj_key in prev.into_iter().chain(next.into_iter()) {
                    let adj_segment = self
                        .segments
                        .get_mut(adj_key)
                        .expect("active segment not found in storage");
                    if let Some(adj_intersection) = segment.geom.intersect_line(&adj_segment.geom) {
                        debug!("Found intersection:\n\tsegment1: {:?}\n\tsegment2: {:?}\n\tintersection: {:?}", segment, adj_segment, adj_intersection);
                        // 1. Split adj_segment, and extra splits to storage
                        let adj_overlap_key = self.adjust_one_segment(adj_key, adj_intersection);

                        // Can't have overlap with a point
                        debug_assert!(adj_overlap_key.is_none());
                    }
                }

                // Points need not be active segments.
                // Send the point-segment to callback.
                cb(segment.into_crossing(event.ty));
            }
            PointRight => {
                // Nothing to do. We could remove this variant once we
                // are confident about the logic.
            }
        }
        true
    }

    /// Process the next event in heap.
    ///
    /// Calls the callback unless the event is spurious.
    #[inline]
    pub fn next_event<F: FnMut(Crossing<C>)>(
        &mut self,
        mut cb: F,
    ) -> Option<SweepPoint<C::Scalar>> {
        self.events.pop().map(|event| {
            let pt = event.point;
            self.handle_event(event, &mut cb);

            pt
        })
    }

    /// Peek and return the next point in the sweep.
    #[inline]
    pub fn peek_point(&self) -> Option<SweepPoint<C::Scalar>> {
        self.events.peek().map(|e| e.point)
    }
}

/// Stores the type of split and extra geometries from adjusting a
/// segment for intersection.
#[derive(Debug)]
enum SplitSegments<T: GeoFloat> {
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
                isec: Some(slab[0].geom),
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
