use std::cmp::Ordering;

use geo::{
    kernels::{HasKernel, Kernel, Orientation},
    line_intersection::LineIntersection,
    Coordinate, GeoFloat, Line,
};

use crate::events::SweepPoint;

/// Either a line segment or a point.
///
/// The coordinates are ordered (see [`SweepPoint`]) and a line
/// segment must have distinct points (use the `Point` variant if the
/// coordinates are the equal).
#[derive(Debug, Clone, Copy)]
pub enum LineOrPoint<T: GeoFloat> {
    Point(SweepPoint<T>),
    Line(SweepPoint<T>, SweepPoint<T>),
}

/// Convert from a [`Line`] ensuring end point ordering.
impl<T: GeoFloat> From<Line<T>> for LineOrPoint<T> {
    fn from(l: Line<T>) -> Self {
        let start = l.start.into();
        let end = l.end.into();
        if start < end {
            LineOrPoint::Line(start, end)
        } else if start > end {
            LineOrPoint::Line(end, start)
        } else {
            LineOrPoint::Point(start)
        }
    }
}

/// Convert from a [`Coordinate`]
impl<T: GeoFloat> From<Coordinate<T>> for LineOrPoint<T> {
    fn from(c: Coordinate<T>) -> Self {
        LineOrPoint::Point(c.into()).into()
    }
}

impl<T: GeoFloat> LineOrPoint<T> {
    /// Return a [`Line`] if it is one, otherwise `None`.
    fn line(&self) -> Option<Line<T>> {
        match self {
            LineOrPoint::Line(p, q) => Some(Line::new(p.0, q.0)),
            _ => None,
        }
    }

    /// Return a [`Coordinate`] if it is one, otherwise `None`.
    fn point(&self) -> Option<Coordinate<T>> {
        match self {
            LineOrPoint::Point(p) => Some(p.0),
            _ => None,
        }
    }

    /// Intersect a line with self and return a point, a overlapping segment or `None`.
    ///
    /// The other argument must be a line variant (panics otherwise).
    pub fn intersect_line(&self, other: &Self) -> Option<Self> {
        let line = other
            .line()
            .expect("tried to intersect_line with a point argument");
        match *self {
            LineOrPoint::Point(p) => {
                if <T as HasKernel>::Ker::orient2d(line.start, p.0, line.end)
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
                line_intersection(Line::new(p.0, q.0), line).map(|l| match l {
                    LineIntersection::SinglePoint { intersection, .. } => intersection.into(),
                    LineIntersection::Collinear { intersection } => intersection.into(),
                })
            }
        }
    }
}

/// Equality based on ordering defined for segments as per algorithm.
impl<T: GeoFloat> PartialEq for LineOrPoint<T> {
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

/// Helper to convert orientation-2d into an ordering
fn orientation_as_ordering(orientation: Orientation) -> Ordering {
    match orientation {
        Orientation::CounterClockwise => Ordering::Less,
        Orientation::Clockwise => Ordering::Greater,
        Orientation::Collinear => Ordering::Equal,
    }
}
