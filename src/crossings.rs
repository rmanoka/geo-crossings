use geo::{
    line_intersection::{line_intersection, LineIntersection},
    Coordinate, GeoFloat, Line,
};
use std::{fmt::Debug, iter::FromIterator};

use crate::sweep::Sweep;

/// Interface for types that can be processed to detect crossings.
///
/// This type is implemented by [`Line`], but users may also implement
/// this on custom types to store extra information.
pub trait Crossable: Sized + Debug {
    /// Scalar used the coordinates.
    type Scalar: GeoFloat;

    /// The geometry associated with this type. Use a `Line` with the
    /// `start` and `end` coordinates to represent a point.
    fn line(&self) -> Line<Self::Scalar>;
}

impl<T: GeoFloat> Crossable for Line<T> {
    type Scalar = T;

    fn line(&self) -> Line<Self::Scalar> {
        *self
    }
}

/// A segment of a input [`Crossable`] type.
///
/// This type is used to convey the part of the input geometry that is
/// intersecting at a given intersection. This is returned by the
/// [`CrossingsIter::intersections`] method.
pub struct Crossing<'a, C: Crossable> {
    /// The input associated with this segment.
    pub crossable: &'a C,

    /// The geometry of this segment.
    ///
    /// This is a part of the input `crossable` geometry and either
    /// starts or ends at the intersection point last yielded by
    /// [`CrossingsIter`]. If it ends at the point (`at_left` is
    /// `false`), then it is guaranteed to not contain any other
    /// intersection point in its interior.
    pub line: Line<C::Scalar>,

    /// Whether this is the first segment of the input line.
    pub first_segment: bool,

    /// Flag that is `true` if the next geom in the sequence overlaps
    /// (i.e. intersects at more than one point) with this. Not
    /// relevant and `false` if this is a point.
    pub has_overlap: bool,

    /// Flag that is `true` if the `geom` starts at the intersection
    /// point. Otherwise, it ends at the intersection point.
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

pub struct Intersections<'a, C: Crossable> {
    inner: CrossingsIter<'a, C>,
    idx: usize,
    jdx: usize,
    is_overlap: bool,
    pt: Option<Coordinate<C::Scalar>>,
}

impl<'a, C: Crossable> FromIterator<&'a C> for Intersections<'a, C> {
    fn from_iter<T: IntoIterator<Item = &'a C>>(iter: T) -> Self {
        Self {
            inner: FromIterator::from_iter(iter),
            idx: 0,
            jdx: 0,
            is_overlap: false,
            pt: None,
        }
    }
}

impl<'a, C: Crossable> Intersections<'a, C> {
    fn intersection(&mut self) -> Option<(&'a C, &'a C, LineIntersection<C::Scalar>)> {
        let (si, sj) = {
            let segments = self.inner.intersections();
            (&segments[self.idx], &segments[self.jdx])
        };
        // Ignore intersections that have already been processed
        let should_compute = if self.is_overlap {
            // For overlap, we only return intersection if at least
            // one segment is the first, and both are at left
            debug_assert_eq!(si.at_left, sj.at_left);
            si.at_left && (si.first_segment || sj.first_segment)
        } else {
            (!si.at_left || si.first_segment) && (!sj.at_left || sj.first_segment)
        };

        should_compute.then(|| {
            let si = si.crossable;
            let sj = sj.crossable;

            (
                si,
                sj,
                line_intersection(si.line(), sj.line())
                    .expect("line_intersection returned `None` disagreeing with `CrossingsIter`"),
            )
        })
    }

    fn step(&mut self) -> bool {
        let seg_len = self.inner.intersections().len();
        if 1 + self.jdx < seg_len {
            self.is_overlap = self.is_overlap && self.inner.intersections()[self.jdx].has_overlap;
            self.jdx += 1;
        } else {
            self.idx += 1;
            if 1 + self.idx >= seg_len {
                loop {
                    self.pt = self.inner.next();
                    if self.pt.is_none() {
                        return false;
                    }
                    if self.inner.intersections().len() > 1 {
                        break;
                    }
                }
                self.idx = 0;
            }
            self.is_overlap = self.inner.intersections()[self.idx].has_overlap;
            self.jdx = self.idx + 1;
        }
        true
    }
}

impl<'a, C: Crossable> Iterator for Intersections<'a, C> {
    type Item = (&'a C, &'a C, LineIntersection<C::Scalar>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if !self.step() {
                return None;
            }

            if let Some(result) = self.intersection() {
                return Some(result);
            }
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
        assert_eq!(iter.count(), 5);
    }

    #[test]
    fn overlap_intersect() {
        init_log();

        fn pp_line(line: &Line<f64>) -> String {
            format!("Line {{({}, {}) - ({}, {})}}", line.start.x, line.start.y, line.end.x, line.end.y)
        }

        let input = vec![
            Line::from([(0., 0.), (1., 1.)]),
            [(1., 0.), (0., 1.)].into(),
            [(0., 0.5), (1., 0.5)].into(),
            [(-1., 0.5), (0.5, 0.5)].into(),
            [(0.5, 0.5), (0.5, 0.5)].into(),
            [(0., 0.), (0., 0.)].into(),
        ];
        // Intersections (by_idx):
        // (0, 1), (0, 2), (0, 3), (0, 4), (0, 5),
        // (1, 2), (1, 3), (1, 4),
        // (2, 3)

        let iter: Intersections<_> = input.iter().collect();
        let count = iter.inspect(|(a, b, int)| {
            eprintln!("{} intersects {}\n\t{:?}", pp_line(a), pp_line(b), int);
        }).count();
        assert_eq!(count, 9);
    }
}
