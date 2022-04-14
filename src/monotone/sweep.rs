use geo::{
    coords_iter::CoordsIter, kernels::Kernel, winding_order::{Winding, WindingOrder}, Coordinate, GeoNum,
    LineString,
};
use log::{info, debug};
use pin_project_lite::pin_project;
use slab::Slab;
use std::{collections::BTreeSet, marker::PhantomPinned, pin::Pin};

use super::{winding_order_from_orientation, Intersection, Segment, VertexType};

use crate::{
    events::{Event, EventType},
    segments::{ActiveSegment, SegmentAccess},
    SweepPoint,
};

pin_project! {
    pub struct Sweep<T: GeoNum> {
        #[pin]
        segments: Slab<Segment<T>>,
        pt_key: usize,
        events: Vec<Event<T>>,
        active_segments: BTreeSet<ActiveSegment<Segment<T>>>,
        _pin: PhantomPinned,
    }
}

impl<T: GeoNum + Unpin> Sweep<T> {
    pub fn from_closed_ring(mut ring: LineString<T>) -> Self {
        ring.close();
        assert!(ring.coords_count() > 3);

        let mut segments = Slab::with_capacity(ring.coords_count() + 1);
        let mut events = Vec::with_capacity(2 * ring.coords_count());

        let winding = ring.winding_order().expect("ring has a winding order");
        debug!("input winding: {winding:?}");
        let entry = segments.vacant_entry();
        let pt_key = entry.key();
        assert_eq!(pt_key, 0);

        entry.insert(Segment::new_point(pt_key, Coordinate::zero()));
        for line in ring.lines() {
            // debug!("processing: {line:?}");
            if line.start == line.end {
                continue;
            }

            let entry = segments.vacant_entry();
            let segment = Segment::new(entry.key(), line, winding.clone());
            for ev in entry.insert(segment).events() {
                events.push(ev);
            }
        }

        events.sort_unstable();
        Sweep {
            segments,
            events,
            pt_key,
            active_segments: BTreeSet::new(),
            _pin: PhantomPinned,
        }
    }

    pub fn process_event(mut self: Pin<&mut Self>) -> Option<()> {
        let (pt, segs) = match self.get_next_segments() {
            Some(e) => e,
            None => return None,
        };
        debug!("process_event:");
        debug!("\tpt: {pt:?}");

        let ixn = self.as_mut().find_intersection_type(pt, segs);
        debug!("\tixn: {ixn:?}");

        // Setup point-segment as temporary active-segment to query segments
        self.as_mut().store_point(pt.coord());

        let mut handle_remove = |e: &Event<T>| {
            if !matches!(e.ty, EventType::LineRight) {
                return;
            }
            // If the segment to be removed has a helper, we
            // process it.
            let mut segment = self.as_mut().storage_index_mut(e.segment_key);
            // &self.segments[e.segment_key];
            if let Some(helper) = &segment.helper() {
                if helper.is_merge() {
                    info!(
                        "R merging: {pt1:?} -> {pt2:?}",
                        pt1 = pt,
                        pt2 = helper.point()
                    );
                    segment.helper_mut().unwrap().reset_merge();
                }
            }
            let this = self.as_mut().project();
            this.active_segments
                .remove_segment(e.segment_key, &this.segments);
        };

        handle_remove(&ixn.event_1);
        handle_remove(&ixn.event_2);

        let winding = {
            let s1 = &self.segments[ixn.event_1.segment_key];
            let s2 = &self.segments[ixn.event_2.segment_key];
            if matches!(ixn.ty, VertexType::Continue) {
                assert_eq!(&s1.interior_winding(), &s2.interior_winding(), "interior winding consistency");
            }
            s1.interior_winding()
        };

        let mut fix_up = |force: bool, is_merge: bool| {
            let next = {
                let this = self.as_mut().project();
                let next = this
                    .active_segments
                    .next_key(*this.pt_key, &this.segments)
                    .expect("fix-up: next_key should be available");
                next
            };
            let mut segment = self.as_mut().storage_index_mut(next);
            let helper = segment.helper().expect("fix-up: next-segment must have helper");
            if helper.is_merge() || force {
                info!(
                    "F merging[f={force}]: {pt1:?} -> {pt2:?}",
                    pt1 = pt,
                    pt2 = helper.point()
                );
                if !is_merge {
                    segment.helper_mut().unwrap().reset_merge();
                }
            }
            let helper = segment.helper_mut().unwrap();
            *helper.point_mut() = pt;
            if is_merge {
                helper.set_merge();
            }
        };

        match ixn.ty {
            VertexType::Continue => {
                if winding == WindingOrder::CounterClockwise {
                    fix_up(false, false);
                }
            }
            VertexType::Split => fix_up(true, false),
            VertexType::Merge => fix_up(false, true),
            _ => {}
        }

        let mut handle_insert = |e: &Event<T>| {
            if !matches!(e.ty, EventType::LineLeft) {
                return;
            }
            let this = self.as_mut().project();
            // No need to explicitly set the segment.helper as the
            // default values set in the constructor are appropriate.
            unsafe {
                this.active_segments
                    .add_segment(e.segment_key, &this.segments);
            }
        };
        handle_insert(&ixn.event_1);
        handle_insert(&ixn.event_2);
        Some(())
    }
}

impl<T: GeoNum + Unpin> Sweep<T> {
    fn store_point(self: Pin<&mut Self>, coord: Coordinate<T>) {
        let pt_key = self.pt_key;
        *self.storage_index_mut(pt_key).get_mut() = Segment::new_point(pt_key, coord);
    }

    fn storage_index_mut(self: Pin<&mut Self>, key: usize) -> Pin<&mut Segment<T>> {
        unsafe { self.map_unchecked_mut(|s| &mut s.segments[key]) }
    }

    fn get_next_segments(&self) -> Option<(SweepPoint<T>, usize)> {
        let events_left = self.events.len();
        if events_left == 0 {
            return None;
        }
        let mut start_idx = events_left - 1;
        let next_point = self.events[start_idx].point;

        while start_idx > 0 {
            if self.events[start_idx - 1].point != next_point {
                break;
            }
            start_idx -= 1;
        }

        Some((next_point, start_idx))
    }

    fn find_intersection_type(
        self: Pin<&mut Self>,
        pt: SweepPoint<T>,
        start_idx: usize,
    ) -> Intersection<T> {
        assert_eq!(
            start_idx,
            self.events.len() - 2,
            "exactly two events at each point"
        );
        let this = self.project();
        let e1 = this.events.pop().unwrap();
        let e2 = this.events.pop().unwrap();
        let s1 = &this.segments[e1.segment_key];
        debug_assert!(s1.geom.is_line());
        let s2 = &this.segments[e2.segment_key];
        debug_assert!(s2.geom.is_line());
        let other1 = match e1.ty {
            EventType::LineLeft => s1.geom.line().end,
            EventType::LineRight => s1.geom.line().start,
            _ => unreachable!("unexpected: point type event!"),
        };
        let other2 = match e2.ty {
            EventType::LineLeft => s2.geom.line().end,
            EventType::LineRight => s2.geom.line().start,
            _ => unreachable!("unexpected: point type event!"),
        };
        debug_assert_eq!(pt, e1.point);
        debug_assert_eq!(pt, e2.point);

        let orientation = T::Ker::orient2d(other1, pt.coord(), other2);
        let winding = winding_order_from_orientation(orientation);
        let int_winding = s1.interior_winding();
        debug!("\tsegment1: {s1:?}");
        debug!("\tint_winding: {int_winding:?}");
        debug!("\twinding: {winding:?}");
        let ty = match (e1.ty, e2.ty) {
            (EventType::LineRight, EventType::LineRight) => {
                let winding = winding.expect("pair RR: can't have collinearity");
                if winding == int_winding {
                    VertexType::End
                } else {
                    VertexType::Merge
                }
            }
            (EventType::LineRight, EventType::LineLeft)
            | (EventType::LineLeft, EventType::LineRight) => VertexType::Continue,
            (EventType::LineLeft, EventType::LineLeft) => {
                let winding = winding.expect("pair RR: can't have collinearity");
                if winding != int_winding {
                    VertexType::Start
                } else {
                    VertexType::Split
                }
            }
            _ => unreachable!("unexpected: point type event!"),
        };

        Intersection {
            ty,
            event_1: e1,
            event_2: e2,
            orientation,
            interior: int_winding,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_simple() {
        env_logger::init();
        let ring = LineString(vec![
            Coordinate::<f64>::from([0., 0.]),
            [-1., 1.].into(),
            [0., 2.].into(),
            [2., 0.].into(),
            [0., -2.].into(),
            [-1., -1.].into(),
        ]);
        let mut sweep = Box::pin(
            Sweep::from_closed_ring(ring)
        );
        while sweep.as_mut().process_event().is_some() {

        }
    }
}
