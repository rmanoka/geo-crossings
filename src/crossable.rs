use std::fmt::Debug;

use geo::{Coordinate, GeoFloat, Line, Point};

use crate::line_or_point::LineOrPoint;

/// Geometry associated with a [`Crossable`] type.
///
/// This is wrapper around an internal enum that represents either a
/// line segment or a point. Use the [`From`] implementations to
/// convert from corresponding geo types: [`Line`], or [`Point`].
///
/// # Semantics
///
/// Line segment must have two distinct points. The constructors
/// convert a degenerate `Line` into a point variant.
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct CrossableGeom<T: GeoFloat>(pub(crate) LineOrPoint<T>);

impl<T: GeoFloat> CrossableGeom<T> {
    /// Check if the enum is a point.
    #[inline]
    pub fn is_point(&self) -> bool {
        matches!(self.0, LineOrPoint::Point(..))
    }

    /// Return the coordinate if the enum is a point variant.
    pub fn point(&self) -> Option<Coordinate<T>> {
        match self.0 {
            LineOrPoint::Point(p) => Some(p.0),
            LineOrPoint::Line(_, _) => None,
        }
    }

    /// Check if the enum is a line.
    #[inline]
    pub fn is_line(&self) -> bool {
        !self.is_point()
    }

    /// Return the line if the enum is a line variant.
    ///
    /// The returned line may have coordinates swapped from the one
    /// used for construction.
    pub fn line(&self) -> Option<Line<T>> {
        self.0.line()
    }
}

impl<T: GeoFloat> From<Line<T>> for CrossableGeom<T> {
    fn from(l: Line<T>) -> Self {
        Into::<LineOrPoint<_>>::into(l).into()
    }
}

impl<T: GeoFloat> From<Coordinate<T>> for CrossableGeom<T> {
    fn from(c: Coordinate<T>) -> Self {
        Into::<LineOrPoint<_>>::into(c).into()
    }
}

impl<T: GeoFloat> From<Point<T>> for CrossableGeom<T> {
    fn from(p: Point<T>) -> Self {
        p.0.into()
    }
}

impl<T: GeoFloat> From<LineOrPoint<T>> for CrossableGeom<T> {
    fn from(geom: LineOrPoint<T>) -> Self {
        CrossableGeom(geom)
    }
}

/// Interface for types that can be processed to detect crossings.
///
/// This type is implemented by [`Line`] and [`Point`], but users may
/// also implement this on custom types to store extra information.
pub trait Crossable: Sized + Debug {
    /// Scalar used the coordinates.
    type Scalar: GeoFloat;
    /// The geometry associated with this type. Must be a line or a
    /// point.
    fn geom(&self) -> CrossableGeom<Self::Scalar>;
}

impl<T: GeoFloat> Crossable for Coordinate<T> {
    type Scalar = T;

    fn geom(&self) -> CrossableGeom<Self::Scalar> {
        (*self).into()
    }
}

impl<T: GeoFloat> Crossable for Point<T> {
    type Scalar = T;

    fn geom(&self) -> CrossableGeom<Self::Scalar> {
        self.0.geom()
    }
}

impl<T: GeoFloat> Crossable for Line<T> {
    type Scalar = T;

    fn geom(&self) -> CrossableGeom<Self::Scalar> {
        (*self).into()
    }
}

impl<T: GeoFloat> Crossable for CrossableGeom<T> {
    type Scalar = T;

    fn geom(&self) -> CrossableGeom<Self::Scalar> {
        *self
    }
}
