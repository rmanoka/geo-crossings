use std::{
    collections::{BTreeSet, BinaryHeap},
    iter::FromIterator,
};

use slab::Slab;

use crate::{
    events::{Event, EventType, SweepPoint},
    line_or_point::LineOrPoint,
    segments::{ActiveSegment, AdjacentSegments, Segment, SplitSegments},
    Crossable,
};

pub struct Sweep<'a, C: Crossable>
where
    C: Crossable,
{
    segments: Box<Slab<Segment<'a, C>>>,
    events: BinaryHeap<Event<C::Scalar>>,
    active_segments: BTreeSet<ActiveSegment<'a, C>>,
}

impl<'a, C: Crossable> FromIterator<&'a C> for Sweep<'a, C> {
    fn from_iter<T: IntoIterator<Item = &'a C>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut sweep = Sweep::with_capacity({
            let (min_size, max_size) = iter.size_hint();
            max_size.unwrap_or(min_size)
        });

        for cr in iter {
            sweep.create_segment(cr, None, None);
        }

        sweep
    }
}

impl<'a, C: Crossable> Sweep<'a, C> {
    fn with_capacity(size: usize) -> Self {
        Sweep {
            segments: Slab::with_capacity(size).into(),
            events: BinaryHeap::with_capacity(size),
            active_segments: Default::default(),
        }
    }

    fn create_segment(&mut self, crossable: &'a C, geom: Option<LineOrPoint<C::Scalar>>, parent: Option<usize>) -> usize {
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
                let child_segment = self.segments[child_key];

                let new_key = Segment::new(
                    &mut self.segments,
                    child_segment.crossable(),
                    Some(segment_geom),
                ).key();
                self.segments[target_key].overlapping = Some(new_key);

                target_key = new_key;
                child = child_segment.overlapping;
            }
        }
        segment_key
    }

    fn adjust_for_intersection(&mut self, key: usize, adj_intersection: LineOrPoint<C::Scalar>) -> SplitSegments<C::Scalar> {
        let segment = &mut self.segments[key];
        let adjust_output = segment.adjust_for_intersection(adj_intersection);
        let new_geom = segment.geom;
        let mut child = segment.overlapping;
        while let Some(child_key) = child {
            let child_seg = &mut self.segments[child_key];
            child_seg.geom = new_geom;
            child = child_seg.overlapping;
        }
        adjust_output
    }

    fn adjust_one_segment(
        &mut self,
        key: usize,
        adj_intersection: LineOrPoint<C::Scalar>,
    ) -> Option<usize> {
        let adj_segment = &mut self.segments[key];
        let adj_cross = adj_segment.crossable();
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
                self.create_segment(adj_cross, Some(right), Some(key));
                Some(self.create_segment(adj_cross, Some(adj_intersection), Some(key)))
            }
        }
    }

    fn handle_event(&mut self, event: Event<C::Scalar>) {
        use EventType::*;
        use LineOrPoint::*;

        // If a segment was adjusted, we may have spurious event for
        // LineRight that should be ignored.
        let segment = *{
            let maybe_segment = self.segments.get(event.segment_key);
            if let LineRight = event.ty {
                match maybe_segment.map(|s| s.geom) {
                    Some(Line(_, q)) if q == event.point => maybe_segment.unwrap(),
                    _ => return,
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
        };

        let prev = self.active_segments.prev_key(&segment, &self.segments);
        let next = self.active_segments.next_key(&segment, &self.segments);

        match &event.ty {
            LineLeft => {
                for adj_key in prev.into_iter().chain(next.into_iter()) {
                    let adj_segment = self
                        .segments
                        .get_mut(adj_key)
                        .expect("active segment not found in storage");
                    if let Some(adj_intersection) =
                        segment.geom.intersect_line(&adj_segment.geom)
                    {
                        // 1. Split adj_segment, and extra splits to storage
                        let adj_overlap_key = self.adjust_one_segment(adj_key, adj_intersection);

                        // 2. Split segment, adding extra segments as needed.
                        let seg_overlap_key =
                            self.adjust_one_segment(event.segment_key, adj_intersection);

                        // match segment.adjust_for_intersection(adj_intersection) {
                        //     Unchanged { overlap } => todo!(),
                        //     SplitOnce { overlap, right } => todo!(),
                        //     SplitTwice { right } => todo!(),
                        // }
                    }
                }

                // Add current segment as active
                self.segments[event.segment_key] = segment;
                self.active_segments
                    .add_segment(event.segment_key, &self.segments);
            }
            LineRight => {
                match (prev, next) {
                    (Some(prev_key), Some(next_key)) => todo!(),
                    _ => {}
                }
                self.active_segments
                    .remove_segment(event.segment_key, &self.segments);
                self.segments.remove(event.segment_key);
            }
            PointLeft => {
                todo!()
            }
            PointRight => {
                todo!()
            }
        }
        todo!()
    }

    #[allow(dead_code)]
    pub fn next_event(&mut self) -> Option<SweepPoint<C::Scalar>> {
        self.events.pop().map(|event| {
            let pt = event.point;
            self.handle_event(event);

            pt
        })
    }
}
