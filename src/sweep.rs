use std::{
    cell::Cell,
    collections::{BTreeSet, BinaryHeap},
    iter::FromIterator,
};

use geo::Coordinate;
use slab::Slab;

use crate::{
    events::{Event, EventType},
    segments::Segment,
    Crossable,
};

pub struct Sweep<'a, C: Crossable>
where
    C: Crossable,
{
    segments: Slab<Segment<'a, C>>,
    events: BinaryHeap<Event<C::Scalar>>,
    active_segments: BTreeSet<Cell<Segment<'a, C>>>,
}

impl<'a, C: Crossable> FromIterator<&'a C> for Sweep<'a, C> {
    fn from_iter<T: IntoIterator<Item = &'a C>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut sweep = Sweep::with_capacity({
            let (min_size, max_size) = iter.size_hint();
            max_size.unwrap_or(min_size)
        });

        for cr in iter {
            sweep.insert_initial(cr);
        }

        sweep
    }
}

impl<'a, C: Crossable> Sweep<'a, C> {
    fn with_capacity(size: usize) -> Self {
        Sweep {
            segments: Slab::with_capacity(size),
            events: BinaryHeap::with_capacity(size),
            active_segments: Default::default(),
        }
    }

    fn insert_initial(&mut self, crossable: &'a C) {
        let segment = Segment::create_in_slab(&mut self.segments, crossable);
        match segment.geom() {
            crate::segments::LineOrPoint::Point(p) => {
                self.events.push(Event {
                    point: p,
                    ty: EventType::PointLeft,
                    segment_key: segment.key(),
                });
                self.events.push(Event {
                    point: p,
                    ty: EventType::PointRight,
                    segment_key: segment.key(),
                });
            }
            crate::segments::LineOrPoint::Line(p, q) => {
                self.events.push(Event {
                    point: p,
                    ty: EventType::LineLeft,
                    segment_key: segment.key(),
                });
                self.events.push(Event {
                    point: q,
                    ty: EventType::LineRight,
                    segment_key: segment.key(),
                });
            }
        }
    }

    fn next_event(&mut self) -> Option<Coordinate<C::Scalar>> {
        self.events.pop().map(|event| {
            // TODO: Handle event
            event.point.0
        })
    }
}
