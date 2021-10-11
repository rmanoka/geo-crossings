//! Implements the line crossings algorithm.
//!
//! # Line Crossings
//!
//! This is an implementation of the [Bentley-Ottman] algorithm to
//! efficiently compute all intersection points of a collection of
//! line segments and points. The iteration yields end-point(s) of the
//! inputs, intersection points, and end-points of overlapping
//! segments.
//!
//! ## Usage
//!
//! Construct a [`CrossingsIter`] from a collection of refs to any
//! type implementing the [`Crossable`] trait. The geo-type [`Line`]
//! implements this trait.
//!
//! ```rust
//! use geo::Line;
//! use geo_crossings::CrossingsIter;
//! let input = vec![
//!     Line::from([(1., 0.), (0., 1.)]),
//!     Line::from([(0., 0.), (1., 1.)]),
//! ];
//! let iter: CrossingsIter<_> = input.iter().collect();
//!
//! // 5 points: 4 end points, and 1 intersection
//! assert_eq!(iter.count(), 5);
//! ```
//!
//! During iteration, use [`CrossingsIter::intersections`] to get the
//! segments that start or end at last yielded point. See [`Crossing`]
//! for more details on information obtained about each segment.
//!
//! [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
//! [`Line`]: geo::Line
//! [`Point`]: geo::Point
//! [`Coordinate`]: geo::Coordinate
mod events;
mod line_or_point;
mod segments;
mod sweep;

pub mod crossings;
pub use crossings::{Crossable, Crossing, CrossingsIter, Intersections};
