use std::cmp::Ordering;

use geo::{
    kernels::{Kernel, Orientation},
    GeoFloat,
};
use slab::Slab;

use crate::{crossable::Crossable, events::SweepPoint};

#[derive(Debug, Clone, Copy)]
pub enum LineOrPoint<T: GeoFloat> {
    Point(SweepPoint<T>),
    Line(SweepPoint<T>, SweepPoint<T>),
}

impl<T: GeoFloat> PartialEq for LineOrPoint<T> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}
impl<T: GeoFloat> PartialOrd for LineOrPoint<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LineOrPoint::Point(p), LineOrPoint::Point(q)) => {
                if p == q {
                    Some(Ordering::Equal)
                } else {
                    // Unequal points do not satisfy pre-condition and
                    // can't be ordered.
                    None
                }
            }
            (LineOrPoint::Point(_), LineOrPoint::Line(_, _)) => {
                other.partial_cmp(self).map(Ordering::reverse)
            }
            (LineOrPoint::Line(p, q), LineOrPoint::Point(r)) => {
                if r > q || p > r {
                    return None;
                }
                Some(
                    orientation_as_ordering(T::Ker::orient2d(p.0, q.0, r.0))
                        .then(Ordering::Greater),
                )
            }
            (LineOrPoint::Line(p1, q1), LineOrPoint::Line(p2, q2)) => {
                if p1 > p2 {
                    return other.partial_cmp(self).map(Ordering::reverse);
                }
                if p1 >= q2 || p2 >= q2 {
                    return None;
                }

                // Assertion: p1 <= p2
                // Assertion: pi < q_j
                Some(
                    orientation_as_ordering(T::Ker::orient2d(p1.0, q1.0, p2.0))
                        .then_with(|| orientation_as_ordering(T::Ker::orient2d(p1.0, q1.0, q2.0))),
                )
            }
        }
    }
}

fn orientation_as_ordering(orientation: Orientation) -> Ordering {
    match orientation {
        Orientation::CounterClockwise => Ordering::Less,
        Orientation::Clockwise => Ordering::Greater,
        Orientation::Collinear => Ordering::Equal,
    }
}

#[derive(Debug)]
pub struct Segment<'a, C: Crossable> {
    geom: LineOrPoint<C::Scalar>,
    key: usize,
    crossable: &'a C,
}

impl<'a, C: Crossable> Segment<'a, C> {
    pub fn create_in_slab(storage: &mut Slab<Self>, crossable: &'a C) -> Self {
        let geom = crossable.geom().0;
        let entry = storage.vacant_entry();

        let segment = Segment {
            key: entry.key(),
            crossable,
            geom,
        };
        entry.insert(segment);
        segment
    }

    /// Get the segment's geom.
    #[inline]
    pub fn geom(&self) -> LineOrPoint<C::Scalar> {
        self.geom
    }

    /// Get the segment's key.
    #[inline]
    pub fn key(&self) -> usize {
        self.key
    }
}

impl<'a, C: Crossable> Clone for Segment<'a, C> {
    fn clone(&self) -> Self {
        Self {
            geom: self.geom.clone(),
            key: self.key.clone(),
            crossable: self.crossable.clone(),
        }
    }
}
impl<'a, C: Crossable> Copy for Segment<'a, C> {}

/// Equality based on key
impl<'a, C: Crossable> PartialEq for Segment<'a, C> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl<'a, C: Crossable> Eq for Segment<'a, C> {}

impl<'a, C: Crossable> PartialOrd for Segment<'a, C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.geom
            .partial_cmp(&other.geom)
            .map(|o| o.then_with(|| self.key.cmp(&other.key)))
    }
}

impl<'a, C: Crossable> Ord for Segment<'a, C> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
