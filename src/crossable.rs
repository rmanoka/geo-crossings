use geo::{GeoFloat, Line, Point};

use crate::segments::LineOrPoint;

#[derive(Debug, Clone, PartialEq)]
pub struct CrossableGeom<T: GeoFloat>(pub(crate) LineOrPoint<T>);

impl<T: GeoFloat> From<Line<T>> for CrossableGeom<T> {
    fn from(l: Line<T>) -> Self {
        let start = l.start.into();
        let end = l.end.into();
        if start < end {
            LineOrPoint::Line(start, end)
        } else if start > end {
            LineOrPoint::Line(end, start)
        } else {
            LineOrPoint::Point(start).into()
        }
        .into()
    }
}

impl<T: GeoFloat> From<Point<T>> for CrossableGeom<T> {
    fn from(p: Point<T>) -> Self {
        LineOrPoint::Point(p.0.into()).into()
    }
}

impl<T: GeoFloat> From<LineOrPoint<T>> for CrossableGeom<T> {
    fn from(geom: LineOrPoint<T>) -> Self {
        CrossableGeom(geom)
    }
}

/// Interface for types that can be processed to detect crossings.
pub trait Crossable: Sized {
    type Scalar: GeoFloat;
    fn geom(&self) -> CrossableGeom<Self::Scalar>;
}
