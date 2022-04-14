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
    /// Checks if the variant is a line.
    #[inline]
    pub fn is_line(&self) -> bool {
        match self {
            LineOrPoint::Line(_, _) => true,
            _ => false,
        }
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
impl<T: GeoFloat> PartialEq for LineOrPoint<T> {
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
