use geo::{
    line_intersection::{line_intersection, LineIntersection},
    Coordinate, GeoFloat, Line,
};
use log::trace;
use std::{fmt::Debug, iter::FromIterator, rc::Rc, sync::Arc};

mod sweep;
pub(crate) use sweep::Sweep;

use crate::{line_or_point::Float, LineOrPoint};
/// Interface for types that can be processed to detect crossings.
///
/// This type is implemented by [`Line`], but users may also implement
/// this on custom types to store extra information.
///
/// # Cloning
///
/// Note that for usage with the crossing iterators, the type must
/// also impl. `Clone`. If the custom type is not cheap to clone, use
/// either a reference to the type, a [`Rc`] or an [`Arc`]. All these
/// are supported via blanket trait implementations.
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

macro_rules! blanket_impl_smart_pointer {
    ($ty:ty) => {
        impl<T: Crossable> Crossable for $ty {
            type Scalar = T::Scalar;

            fn line(&self) -> Line<Self::Scalar> {
                T::line(self)
            }
        }
    };
}

impl<'a, T: Crossable> Crossable for &'a T {
    type Scalar = T::Scalar;

    fn line(&self) -> Line<Self::Scalar> {
        T::line(*self)
    }
}

blanket_impl_smart_pointer!(Box<T>);
blanket_impl_smart_pointer!(Rc<T>);
blanket_impl_smart_pointer!(Arc<T>);

/// A segment of a input [`Crossable`] type.
///
/// This type is used to convey the part of the input geometry that is
/// intersecting at a given intersection. This is returned by the
/// [`CrossingsIter::intersections`] method.
#[derive(Debug, Clone, Copy)]
pub struct Crossing<C: Crossable> {
    /// The input associated with this segment.
    pub crossable: C,

    /// The geometry of this segment.
    ///
    /// This is a part of the input `crossable` geometry and either
    /// starts or ends at the intersection point last yielded by
    /// [`CrossingsIter`]. If it ends at the point (`at_left` is
    /// `false`), then it is guaranteed to not contain any other
    /// intersection point in its interior.
    pub line: LineOrPoint<C::Scalar>,

    /// Whether this is the first segment of the input line.
    pub first_segment: bool,

    /// Flag that is `true` if the next geom in the sequence overlaps
    /// (i.e. intersects at more than one point) with this. Not
    /// relevant and `false` if this is a point.
    ///
    /// Note that the overlapping segments may not always
    /// _all_ get batched together. They may be reported as
    /// one or more set of overlapping segments in an
    /// arbitrary order.
    pub has_overlap: bool,

    /// Flag that is `true` if the `geom` starts at the intersection
    /// point. Otherwise, it ends at the intersection point.
    pub at_left: bool,

    pub(crate) key: usize,
}

/// Iterator that yields all crossings.
///
/// Yields all end points, intersections and overlaps of a set of
/// line-segments and points. Construct it by `collect`-ing an
/// iterator of [`Crossable`]. The implementation uses the
/// [Bentley-Ottman] algorithm and runs in time O((n + k) log n) time;
/// this is faster than a brute-force search for intersections across
/// all pairs of input segments if k --- the number of intersections
/// --- is small compared to n^2.
///
/// ## Usage
///
/// Construct from an iterator of any type implementing the
/// [`Crossable`] trait. Use the [`CrossingsIter::intersections`]
/// method to access all segments that start or end at the last
/// yielded point.
///
/// ```rust
/// use geo::Line;
/// use geo_crossings::CrossingsIter;
/// use std::iter::FromIterator;
/// let input = vec![
///     Line::from([(1., 0.), (0., 1.)]),
///     Line::from([(0., 0.75), (1., 0.25)]),
///     Line::from([(0., 0.25), (1., 0.75)]),
///     Line::from([(0., 0.), (1., 1.)]),
/// ];
/// let iter = CrossingsIter::<_>::from_iter(input);
/// // 1 intersection point, and 8 end points
/// assert_eq!(iter.count(), 9);
/// ```
///
/// [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
pub struct CrossingsIter<C>
where
    C: Crossable + Clone,
{
    sweep: Sweep<C>,
    segments: Vec<Crossing<C>>,
}

impl<C> CrossingsIter<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    /// Returns the segments that intersect the last point yielded by
    /// the iterator.
    pub fn intersections_mut(&mut self) -> &mut [Crossing<C>] {
        &mut self.segments
    }

    pub fn intersections(&self) -> &[Crossing<C>] {
        &self.segments
    }

    pub(crate) fn prev_active(&self, c: &Crossing<C>) -> Option<&C> {
        self.sweep.prev_active(c).map(|s| s.crossable())
    }
}

impl<C> FromIterator<C> for CrossingsIter<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
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

impl<C> Iterator for CrossingsIter<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    type Item = Coordinate<C::Scalar>;

    fn next(&mut self) -> Option<Self::Item> {
        let segments = &mut self.segments;

        segments.clear();
        let mut last_point = self.sweep.peek_point();
        trace!("pt: {last_point:?}");
        while last_point == self.sweep.peek_point() && self.sweep.peek_point().is_some() {
            last_point = self.sweep.next_event(|seg, ty| {
                trace!(
                    "cb: {seg:?} {ty:?} (crossable = {cross:?})",
                    cross = LineOrPoint::from(seg.crossable().line())
                );
                segments.push(Crossing::from_segment(seg, ty))
            });
        }

        if segments.is_empty() {
            None
        } else {
            last_point.map(|p| p.coord())
        }
    }
}

/// Iterator over all intersections of a collection of lines.
///
/// Yields tuples `(C, C, LineIntersection)` for each pair of input
/// crossables that intersect or overlap. This is a drop-in
/// replacement for computing [`LineIntersection`] over all pairs of
/// the collection, but is typically more efficient. The
/// implementation uses the [Bentley-Ottman] algorithm and runs in
/// time O((n + k) log n) time; this is faster than a brute-force
/// search for intersections across all pairs of input segments if k,
/// the number of intersections is small compared to n^2.
///
/// ## Usage
///
/// Construct from an iterator of any type implementing the
/// [`Crossable`] trait. The geo-type [`Line`] implements this trait.
/// See the trait documentation for more information on usage with
/// custom types.
///
/// ```rust
/// use geo::Line;
/// use geo_crossings::Intersections;
/// use std::iter::FromIterator;
/// let input = vec![
///     Line::from([(1., 0.), (0., 1.)]),
///     Line::from([(0., 0.75), (1., 0.25)]),
///     Line::from([(0., 0.25), (1., 0.75)]),
///     Line::from([(0., 0.), (1., 1.)]),
/// ];
/// let iter = Intersections::<_>::from_iter(input);
/// // All pairs intersect
/// assert_eq!(iter.count(), 6);
/// ```
///
/// [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
pub struct Intersections<C: Crossable + Clone> {
    inner: CrossingsIter<C>,
    idx: usize,
    jdx: usize,
    is_overlap: bool,
    pt: Option<Coordinate<C::Scalar>>,
}

impl<C> FromIterator<C> for Intersections<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        Self {
            inner: FromIterator::from_iter(iter),
            idx: 0,
            jdx: 0,
            is_overlap: false,
            pt: None,
        }
    }
}

impl<C> Intersections<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    fn intersection(&mut self) -> Option<(C, C, LineIntersection<C::Scalar>)> {
        let (si, sj) = {
            let segments = self.inner.intersections_mut();
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

        if should_compute {
            let si = si.crossable.clone();
            let sj = sj.crossable.clone();

            let int = line_intersection(si.line(), sj.line())
                .expect("line_intersection returned `None` disagreeing with `CrossingsIter`");

            Some((si, sj, int))
        } else {
            None
        }
    }

    fn step(&mut self) -> bool {
        let seg_len = self.inner.intersections_mut().len();
        if 1 + self.jdx < seg_len {
            self.is_overlap =
                self.is_overlap && self.inner.intersections_mut()[self.jdx].has_overlap;
            self.jdx += 1;
        } else {
            self.idx += 1;
            if 1 + self.idx >= seg_len {
                loop {
                    self.pt = self.inner.next();
                    if self.pt.is_none() {
                        return false;
                    }
                    if self.inner.intersections_mut().len() > 1 {
                        break;
                    }
                }
                self.idx = 0;
            }
            self.is_overlap = self.inner.intersections_mut()[self.idx].has_overlap;
            self.jdx = self.idx + 1;
        }
        true
    }
}

impl<C> Iterator for Intersections<C>
where
    C: Crossable + Clone,
    C::Scalar: Float,
{
    type Item = (C, C, LineIntersection<C::Scalar>);

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
pub(crate) mod tests {
    use geo::{Line, Polygon, Rect};
    use log::info;
    use std::io::Write;

    use crate::line_or_point::LineOrPoint;

    use super::*;

    pub(crate) fn init_log() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{} - {}", record.level(), record.args()))
            .try_init();
    }

    #[test]
    fn simple_iter() {
        let input = vec![
            Rc::from(Line::from([(1., 0.), (0., 1.)])),
            Line::from([(0., 0.), (1., 1.)]).into(),
        ];
        let iter: CrossingsIter<_> = input.into_iter().collect();
        assert_eq!(iter.count(), 5);
    }

    #[test]
    fn overlap_intersect() {
        init_log();

        fn pp_line(line: &Line<f64>) -> String {
            format!(
                "Line {{({}, {}) - ({}, {})}}",
                line.start.x, line.start.y, line.end.x, line.end.y
            )
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
        let count = iter
            .inspect(|(a, b, int)| {
                eprintln!("{} intersects {}\n\t{:?}", pp_line(a), pp_line(b), int);
            })
            .count();
        assert_eq!(count, 9);
    }

    #[test]
    #[ignore]
    fn check_adhoc_crossings() {
        init_log();

        let mut input = vec![];
        let poly1: Polygon<_> = Rect::new((0., 0.), (1., 1.)).into();
        let poly2: Polygon<_> = Rect::new((0.5, 1.), (2., 2.)).into();
        input.extend(poly1.exterior().lines());
        input.extend(poly2.exterior().lines());

        let mut iter: CrossingsIter<_> = input.into_iter().collect();
        while let Some(pt) = iter.next() {
            info!("pt: {pt:?}");
            iter.intersections_mut().iter().for_each(|i| {
                info!(
                    "\t{geom:?} ({at_left}) {ovl}",
                    geom = LineOrPoint::from(i.line),
                    at_left = if i.at_left { "S" } else { "E" },
                    ovl = if i.has_overlap { "[OVL] " } else { "" },
                );
            });
        }
    }
}
