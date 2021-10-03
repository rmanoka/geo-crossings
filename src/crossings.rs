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
    ///
    /// This is a part of the input `crossable` geometry. The parts
    /// are formed by splitting the input geometry as and when
    /// intersection points are witnessed. If this geometry ends at
    /// the intersection point (`at_left` is `false`), then it is
    /// guaranteed to not contain any intersection point.
    pub geom: CrossableGeom<C::Scalar>,

    /// Whether this is the first segment of the input line (not
    /// relevant if input is a point).
    pub first_segment: bool,

    /// Flag that is `true` if the next geom in the sequence overlaps
    /// with this.
    pub has_overlap: bool,

    /// Flag that is `true` if the `geom` starts at the intersection point.
    pub at_left: bool,
}

/// Iterator that yields all crossings.
///
/// Yields all end points, intersections and overlaps of a set of
/// line-segments and points. Construct it by `collect`-ing an
/// iterator of references to [`Crossable`].
///
/// The implementation uses the [Bentley-Ottman] algorithm and runs in
/// time O((n + k) log(n)) time; this is faster than a brute-force
/// search for intersections across all pairs of input segments if k,
/// the number of intersections is small compared to n^2.
///
/// [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
pub struct CrossingsIter<'a, C: Crossable> {
    sweep: Sweep<'a, C>,
    segments: Vec<Crossing<'a, C>>,
}

impl<'a, C: Crossable> CrossingsIter<'a, C> {
    /// Returns the segments that intersect the last point yielded by
    /// the iterator.
    pub fn intersections(&mut self) -> &mut [Crossing<'a, C>] {
        &mut self.segments
    }
}

impl<'a, C: Crossable> FromIterator<&'a C> for CrossingsIter<'a, C> {
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

impl<'a, C: Crossable> Iterator for CrossingsIter<'a, C> {
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
    fn simple_iter() {
        let input = vec![
            Line::from([(1., 0.), (0., 1.)]),
            Line::from([(0., 0.), (1., 1.)]),
        ];
        let iter: CrossingsIter<_> = input.iter().collect();
        for pt in iter {
            eprintln!("{:?}", pt);
        }
    }

    #[test]
    fn overlap_intersect() {
        init_log();

        let input = vec![
            CrossableGeom::from(Line::from([(0., 0.), (1., 1.)])),
            Line::from([(1., 0.), (0., 1.)]).into(),
            Line::from([(0., 0.5), (1., 0.5)]).into(),
            Line::from([(-1., 0.5), (0.5, 0.5)]).into(),
            Coordinate::from((0.5, 0.5)).into(),
            Coordinate::from((0., 0.)).into(),
        ];

        let mut iter: CrossingsIter<_> = input.iter().collect();
        while let Some(pt) = iter.next() {
            eprintln!("{:?} has {} segments", pt, iter.intersections().len());
        }
    }
}
