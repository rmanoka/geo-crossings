use std::cmp::Ordering;

use float_next_after::NextAfter;
use geo::{
    kernels::{HasKernel, Kernel, Orientation},
    line_intersection::LineIntersection,
    Coordinate, GeoFloat, GeoNum, Line,
};
use log::error;

use crate::events::SweepPoint;

pub trait Float: GeoFloat + NextAfter<Self> {}
impl<T: GeoFloat + NextAfter<T>> Float for T {}


/// Either a line segment or a point.
///
/// The coordinates are ordered (see [`SweepPoint`]) and a line
/// segment must have distinct points (use the `Point` variant if the
/// coordinates are the equal).
#[derive(Debug, Clone, Copy)]
pub enum LineOrPoint<T: GeoNum> {
    Point(SweepPoint<T>),
    Line(SweepPoint<T>, SweepPoint<T>),
}

/// Convert from a [`Line`] ensuring end point ordering.
impl<T: GeoNum> From<Line<T>> for LineOrPoint<T> {
    fn from(l: Line<T>) -> Self {
        let start: SweepPoint<T> = l.start.into();
        let end = l.end.into();
        match start.cmp(&end) {
            Ordering::Less => LineOrPoint::Line(start, end),
            Ordering::Equal => LineOrPoint::Point(start),
            Ordering::Greater => LineOrPoint::Line(end, start),
        }
    }
}

/// Convert from a [`Coordinate`]
impl<T: GeoNum> From<Coordinate<T>> for LineOrPoint<T> {
    fn from(c: Coordinate<T>) -> Self {
        LineOrPoint::Point(c.into())
    }
}

impl<T: GeoNum> LineOrPoint<T> {
    /// Checks if the variant is a line.
    #[inline]
    pub fn is_line(&self) -> bool {
        matches!(self, LineOrPoint::Line(_, _))
    }

    /// Return a [`Line`] if it is one, otherwise `None`.
    #[inline]
    pub fn line(&self) -> Line<T> {
        match self {
            LineOrPoint::Line(p, q) => Line::new(p.coord(), q.coord()),
            LineOrPoint::Point(p) => Line::new(p.coord(), p.coord()),
        }
    }

    /// Returns the lexicographic first coordinate of the geometry.
    pub(crate) fn first(&self) -> SweepPoint<T> {
        *match self {
            LineOrPoint::Point(p) => p,
            LineOrPoint::Line(p, _) => p,
        }
    }

    pub fn second(&self) -> SweepPoint<T> {
        *match self {
            LineOrPoint::Point(p) => p,
            LineOrPoint::Line(_, p) => p,
        }
    }
}
impl<T: GeoFloat> LineOrPoint<T> {
    /// Intersect a line with self and return a point, a overlapping segment or `None`.
    ///
    /// The `other` argument must be a line variant (panics otherwise).
    pub fn intersect_line(&self, other: &Self) -> Option<Self> {
        assert!(other.is_line(), "tried to intersect with a point variant!");

        let line = other.line();
        match *self {
            LineOrPoint::Point(p) => {
                if <T as HasKernel>::Ker::orient2d(line.start, p.coord(), line.end)
                    == Orientation::Collinear
                {
                    let ls = line.start.into();
                    let le = line.end.into();
                    if p >= ls && p <= le {
                        Some(*self)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            LineOrPoint::Line(p, q) => {
                use geo::algorithm::line_intersection::line_intersection;
                line_intersection(Line::new(p.coord(), q.coord()), line).map(|l| match l {
                    LineIntersection::SinglePoint { intersection, .. } => intersection.into(),
                    LineIntersection::Collinear { intersection } => intersection.into(),
                })
            }
        }
    }
}

impl<T: Float> LineOrPoint<T> {

    pub fn intersect_line_ordered(&self, other: &Self) -> Option<Self> {
        match self.intersect_line(other) {
            Some(LineOrPoint::Point(mut pt)) => {
                let (mut x, y) = pt.coord().x_y();

                let c = self.first().coord();

                if x == c.x && y < c.y {
                    x = x.next_after(T::infinity());
                }
                pt = Coordinate { x, y }.into();

                debug_assert!(
                    pt >= self.first(),
                    "intersection not in total-order with first line: {pt:?}\n\tLine({lp1:?} - {lp2:?}) X Line({lp3:?} - {lp4:?})",
                    lp1 = self.first(),
                    lp2 = self.second(),
                    lp3 = other.first(),
                    lp4 = other.second(),
                );
                debug_assert!(
                    pt >= other.first(),
                    "intersection not in total-order with second line: {pt:?}\n\tLine({lp1:?} - {lp2:?}) X Line({lp3:?} - {lp4:?})",
                    lp1 = self.first(),
                    lp2 = self.second(),
                    lp3 = other.first(),
                    lp4 = other.second(),
                );
                // FIXME: line_Intersection may return points
                // that are not "within the line bounds" as per
                // the ordering. This causes issues with the
                // sweep.
                Some(LineOrPoint::Point(pt))
            },
            e => e,
        }
        // if let Some(LineOrPoint::Point(pt)) = &mut lpt {
            // if lp.first() < self.first() {

            // }
        // }
    }

    #[cfg(test)]
    pub fn coords_equal(&self, other: &LineOrPoint<T>) -> bool {
        match (self, other) {
            (LineOrPoint::Point(p), LineOrPoint::Point(q)) => p == q,
            (LineOrPoint::Line(p1, q1), LineOrPoint::Line(p2, q2)) => p1 == p2 && q1 == q2,
            _ => false,
        }
    }
}

/// Equality based on ordering defined for segments as per algorithm.
impl<T: GeoNum> PartialEq for LineOrPoint<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

/// Ordering defined for segments as per algorithm.
///
/// Requires the following conditions:
///
/// 1. If comparing two lines, the both left ends must be strictly
/// smaller than both right ends.
///
/// 2. A point is treated as a infinitesimal small vertical segment
/// centered at its coordinates.
impl<T: GeoNum> PartialOrd for LineOrPoint<T> {
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
                    orientation_as_ordering(T::Ker::orient2d(p.coord(), q.coord(), r.coord()))
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
                    orientation_as_ordering(T::Ker::orient2d(p1.coord(), q1.coord(), p2.coord()))
                        .then_with(|| {
                            orientation_as_ordering(T::Ker::orient2d(
                                p1.coord(),
                                q1.coord(),
                                q2.coord(),
                            ))
                        }),
                )
            }
        }
    }
}

/// Helper to convert orientation-2d into an ordering
#[inline]
fn orientation_as_ordering(orientation: Orientation) -> Ordering {
    match orientation {
        Orientation::CounterClockwise => Ordering::Less,
        Orientation::Clockwise => Ordering::Greater,
        Orientation::Collinear => Ordering::Equal,
    }
}
