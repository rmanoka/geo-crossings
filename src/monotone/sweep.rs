use geo::{
    coords_iter::CoordsIter,
    kernels::Kernel,
    winding_order::{Winding, WindingOrder},
    Coordinate, GeoNum, LineString, Polygon,
};
use log::debug;
use pin_project_lite::pin_project;
use slab::Slab;
use smallvec::SmallVec;
use std::{collections::BTreeSet, marker::PhantomPinned, pin::Pin};

use super::{
    segment::Intersection, segment::Link, segment::VertexType, winding_order_from_orientation,
    Segment,
};

use crate::{
    events::{Event, EventType},
    utils::winding_inverse,
    active::{Active, Access},
    SweepPoint,
};

pin_project! {
    /// Monotone decomposition sweep implementation.
    ///
    /// Implements the plane-sweep used by the polygon
    /// decomposition algorithm. Users should typically use
    /// [`crate::monotone::monotone_chains`].
    ///
    /// Implementation is based on the algorithm description
    /// in these [awesome lecture notes].
    ///
    /// [awesome lecture notes]: https://www.cs.umd.edu/class/spring2020/cmsc754/Lects/lect05-triangulate.pdf
    pub struct Sweep<T: GeoNum> {
        #[pin]
        segments: Slab<Segment<T>>,
        pt_key: usize,
        events: Vec<Event<T>>,
        active_segments: BTreeSet<Active<Segment<T>>>,
        _pin: PhantomPinned,
    }
}
type Links<T> = SmallVec<[Link<T>; 4]>;

#[derive(Debug, Clone)]
pub struct SweepEvent<T: GeoNum> {
    pub pt: SweepPoint<T>,
    pub ty: VertexType,
    pub links: Links<T>,
}

impl<T: GeoNum> Sweep<T> {
    pub fn from_polygon(poly: &Polygon<T>) -> Self {
        let n = poly.coords_count();
        let mut segments = Slab::with_capacity(n + 1);
        let events = Vec::with_capacity(2 * n);

        // Insert dummy segment for point (we never add it to the b-tree).
        let entry = segments.vacant_entry();
        let pt_key = entry.key();
        assert_eq!(pt_key, 0);
        entry.insert(Segment::new_point(pt_key, Coordinate::zero()));

        let mut sweep = Sweep {
            segments,
            events,
            pt_key,
            active_segments: BTreeSet::new(),
            _pin: PhantomPinned,
        };
        sweep.add_closed_ring(poly.exterior(), false);
        for hole in poly.interiors() {
            sweep.add_closed_ring(hole, true);
        }
        sweep.events.sort_unstable();
        sweep
    }
    pub fn process_event(mut self: Pin<&mut Self>) -> Option<SweepEvent<T>> {
        let mut links = Links::new();
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
            let segment = self.as_mut().storage_index_mut(e.segment_key);
            // &self.segments[e.segment_key];
            if let Some(helper) = &segment.helper() {
                if helper.is_merge() {
                    let pt2 = helper.point();
                    // info!("R merging: {pt1:?} -> {pt2:?}", pt1 = pt,);
                    segment.helper_mut().unwrap().reset_merge();
                    links.push(Link::Merge {
                        prev: pt2,
                        next: pt,
                        limbs: None,
                    });
                }
            }
            let this = self.as_mut().project();
            this.active_segments
                .remove_key(e.segment_key, &this.segments);
        };

        handle_remove(&ixn.event_1);
        handle_remove(&ixn.event_2);

        let winding = {
            let s1 = &self.segments[ixn.event_1.segment_key];
            let s2 = &self.segments[ixn.event_2.segment_key];
            if matches!(ixn.ty, VertexType::Continue) {
                assert_eq!(
                    &s1.interior_winding(),
                    &s2.interior_winding(),
                    "interior winding consistency"
                );
            }
            s1.interior_winding()
        };

        let mut fix_up = |force: bool, is_merge: bool| {
            let next = {
                let this = self.as_mut().project();

                this.active_segments
                    .next_key(*this.pt_key, &this.segments)
                    .expect("fix-up: next_key should be available")
            };
            let segment = self.as_mut().storage_index_mut(next);
            let helper = segment
                .helper()
                .expect("fix-up: next-segment must have helper");

            let (top, bot) = if ixn.interior != WindingOrder::Clockwise {
                (ixn.other_1, ixn.other_2)
            } else {
                (ixn.other_2, ixn.other_1)
            };
            if helper.is_merge() || force {
                let pt2 = helper.point();
                // info!("F merging[split={force}]: {pt1:?} -> {pt2:?}", pt1 = pt,);
                links.push(if helper.is_merge() {
                    Link::Merge {
                        prev: pt2,
                        next: pt,
                        limbs: Some((top, bot)),
                    }
                } else {
                    Link::Split {
                        prev: pt2,
                        next: pt,
                        top,
                        bot,
                    }
                });
                if !is_merge {
                    segment.helper_mut().unwrap().reset_merge();
                }
            }
            let segment = self.as_mut().storage_index_mut(next);
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
                let (pt1, pt2) = {
                    if ixn.other_1 > ixn.other_2 {
                        (ixn.other_2, ixn.other_1)
                    } else if ixn.other_1 < ixn.other_2 {
                        (ixn.other_1, ixn.other_2)
                    } else {
                        unreachable!("continue segment: others must be orderable!");
                    }
                };
                links.push(Link::Continue {
                    prev: pt1,
                    curr: pt,
                    next: pt2,
                    interior: ixn.interior,
                });
            }
            VertexType::Split => fix_up(true, false),
            VertexType::Merge => fix_up(false, true),
            VertexType::End => {
                let (top, bot) = if ixn.interior == WindingOrder::Clockwise {
                    (ixn.other_1, ixn.other_2)
                } else {
                    (ixn.other_2, ixn.other_1)
                };
                links.push(Link::End { top, bot, sink: pt });
            }
            VertexType::Start => {
                let (top, bot) = if ixn.interior == WindingOrder::Clockwise {
                    (ixn.other_1, ixn.other_2)
                } else {
                    (ixn.other_2, ixn.other_1)
                };
                links.push(Link::Start { root: pt, top, bot });
            }
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
                    .add_key(e.segment_key, &this.segments);
            }
        };
        handle_insert(&ixn.event_1);
        handle_insert(&ixn.event_2);
        Some(SweepEvent {
            pt,
            links,
            ty: ixn.ty,
        })
    }
}

impl<T: GeoNum> Iterator for Pin<Box<Sweep<T>>> {
    type Item = SweepEvent<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.as_mut().process_event()
    }
}

impl<T: GeoNum> Sweep<T> {
    fn add_closed_ring(&mut self, ring: &LineString<T>, is_hole: bool) {
        assert!(ring.is_closed());
        assert!(ring.coords_count() > 3);

        let winding = {
            let winding = ring.winding_order().expect("ring has a winding order");
            if is_hole {
                winding_inverse(winding)
            } else {
                winding
            }
        };
        debug!("input winding: {winding:?}");
        for line in ring.lines() {
            // debug!("processing: {line:?}");
            if line.start == line.end {
                continue;
            }

            let entry = self.segments.vacant_entry();
            let segment = Segment::new(entry.key(), line, winding.clone());
            for ev in entry.insert(segment).events() {
                self.events.push(ev);
            }
        }
    }
    fn store_point(self: Pin<&mut Self>, coord: Coordinate<T>) {
        let pt_key = self.pt_key;
        // Safety: the pt. segment is never stored as an
        // `ActiveSegment` in the b-tree-set.
        unsafe {
            *self.storage_index_mut(pt_key).get_unchecked_mut() = Segment::new_point(pt_key, coord);
        }
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
        let other_1 = match e1.ty {
            EventType::LineLeft => s1.geom.line().end,
            EventType::LineRight => s1.geom.line().start,
            _ => unreachable!("unexpected: point type event!"),
        };
        let other_2 = match e2.ty {
            EventType::LineLeft => s2.geom.line().end,
            EventType::LineRight => s2.geom.line().start,
            _ => unreachable!("unexpected: point type event!"),
        };
        debug_assert_eq!(pt, e1.point);
        debug_assert_eq!(pt, e2.point);

        let orientation = T::Ker::orient2d(other_1, pt.coord(), other_2);
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
            other_1: other_1.into(),
            other_2: other_2.into(),
            orientation,
            interior: int_winding,
        }
    }
}
