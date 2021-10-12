//! Implements the line crossings algorithm.
//!
//! # Line Crossings
//!
//! This is an implementation of the [Bentley-Ottman] algorithm to
//! efficiently compute all intersections of a collection of line
//! segments and points. The simplest usage is using the
//! [`Intersections`] iterator which yields all intersections and
//! overlaps. This is essentially a drop-in replacement to using
//! [`line_intersection`] over all pairs, but is typically more
//! efficient.
//!
//! For a more advanced usage such as polygon clipping, use
//! [`CrossingsIter`] which iterates over all intersection points,
//! while also providing information about all segments that start or
//! end at the last yielded point.
//!
//! ## Usage
//!
//! Construct a [`Intersections`] from an iterator of any type
//! implementing the [`Crossable`] trait. The geo-type [`Line`]
//! implements this trait. See the trait documentation for more
//! information on usage with custom types.
//!
//! ```rust
//! use geo::Line;
//! use geo_crossings::Intersections;
//! use std::iter::FromIterator;
//! let input = vec![
//!     Line::from([(1., 0.), (0., 1.)]),
//!     Line::from([(0., 0.5), (1., 0.5)]),
//!     Line::from([(0., 0.), (1., 1.)]),
//! ];
//! let iter = Intersections::<_>::from_iter(input);
//! // All pairs intersect
//! assert_eq!(iter.count(), 3);
//! ```
//!
//! [Bentley-Ottman]: //en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
//! [`Line`]: geo::Line
//! [`Point`]: geo::Point
//! [`Coordinate`]: geo::Coordinate
//! [`line_intersection`]: geo::algorithm::line_intersection::line_intersection
mod events;
mod line_or_point;
mod segments;
mod sweep;

pub mod crossings;
pub use crossings::{Crossable, Crossing, CrossingsIter, Intersections};
