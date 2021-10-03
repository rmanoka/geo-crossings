use std::iter::FromIterator;

use geo::Coordinate;

use crate::{sweep::Sweep, Crossable, CrossableGeom};

/// A segment of a [`Crossable`].
///
/// This type is used to convey the part of the input geometry that is
/// intersecting at a given intersection.
pub struct Crossing<'a, C: Crossable> {
    /// The input associated with this segment.
    pub crossable: &'a C,

    /// The geometry of this segment.
    pub geom: CrossableGeom<C::Scalar>,

    /// Whether this is the first segment of the input line (not
    /// relevant if input is a point).
    pub first_segment: bool,

    /// Whether the next segment returned from sweep overlaps with
    /// this.
    pub has_overlap: bool,

    /// Whether the left or the right-end point is at the point of
    /// intersection.
    pub at_left: bool,
}

pub struct CrossingsIterator<'a, C: Crossable> {
    sweep: Sweep<'a, C>,
    segments: Vec<Crossing<'a, C>>,
}

impl<'a, C: Crossable> CrossingsIterator<'a, C> {
    pub fn intersections(&mut self) -> &mut [Crossing<'a, C>] {
        &mut self.segments
    }
}

impl<'a, C: Crossable> FromIterator<&'a C> for CrossingsIterator<'a, C> {
    fn from_iter<T: IntoIterator<Item = &'a C>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let size = {
            let (min_size, max_size) = iter.size_hint();
            max_size.unwrap_or(min_size)
        };
        let sweep = Sweep::new(iter);
        let segments = Vec::with_capacity(4 * size);
        Self { sweep, segments }
    }
}

impl<'a, C: Crossable> Iterator for CrossingsIterator<'a, C> {
    type Item = Coordinate<C::Scalar>;

    fn next(&mut self) -> Option<Self::Item> {
        let segments = &mut self.segments;

        segments.clear();
        let mut last_point = self.sweep.peek_point();
        while last_point == self.sweep.peek_point() && self.sweep.peek_point().is_some() {
            last_point = self.sweep.next_event(|crossing| segments.push(crossing));
        }

        if segments.is_empty() {
            None
        } else {
            last_point.map(|p| p.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use geo::Line;

    use super::*;

    fn init_log() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn simple_intersect() {
        init_log();

        let input = vec![
            CrossableGeom::from(Line::from([(0., 0.), (1., 1.)])),
            Line::from([(1., 0.), (0., 1.)]).into(),
            Line::from([(0., 0.5), (1., 0.5)]).into(),
            Line::from([(-1., 0.5), (0.5, 0.5)]).into(),
            Coordinate::from((0.5, 0.5)).into(),
            Coordinate::from((0., 0.)).into(),
        ];

        let mut iter: CrossingsIterator<_> = input.iter().collect();
        while let Some(pt) = iter.next() {
            eprintln!("{:?} has {} segments", pt, iter.intersections().len());
        }
    }
}
