use std::{cmp::Ordering};
use slab::Slab;

use crate::{crossable::Crossable, events::SweepPoint, line_or_point::LineOrPoint};


/// A segment is an internal [`LineOrPoint`] generated during the
/// sweep.
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
