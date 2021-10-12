use std::collections::{BTreeSet, BinaryHeap};

use log::{debug, trace};
use slab::Slab;

use crate::{
    events::{Event, EventType, SweepPoint},
    line_or_point::LineOrPoint,
    segments::{ActiveSegment, AdjacentSegments, Segment, SplitSegments},
    Crossable, Crossing,
};

pub struct Sweep<C: Crossable>
where
    C: Crossable + Clone,
{
    segments: Box<Slab<Segment<C>>>,
    events: BinaryHeap<Event<C::Scalar>>,
    active_segments: BTreeSet<ActiveSegment<C>>,
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

        let prev = self.active_segments.prev_key(&segment, &self.segments);
        let next = self.active_segments.next_key(&segment, &self.segments);

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
                            assert!(
                                self.handle_event(event, cb),
                                "special right-end event handling failed"
                            )
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
                self.active_segments
                    .add_segment(event.segment_key, &self.segments);

                let mut segment_key = Some(event.segment_key);
                while let Some(key) = segment_key {
                    let segment = &self.segments[key];
                    cb(segment.clone().into_crossing(event.ty));
                    segment_key = segment.overlapping;
                }
            }
            LineRight => {
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
                        assert!(
                            self.adjust_one_segment(prev_key, adj_intersection)
                                .is_none()
                                && self
                                    .adjust_one_segment(next_key, adj_intersection)
                                    .is_none(),
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
                        assert!(adj_overlap_key.is_none());
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
    pub fn peek_point(&self) -> Option<SweepPoint<C::Scalar>> {
        self.events.peek().map(|e| e.point)
    }
}
