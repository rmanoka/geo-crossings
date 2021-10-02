mod events;
mod line_or_point;
mod segments;
mod sweep;

pub mod crossable;
pub use crossable::{Crossable, CrossableGeom};

pub mod crossings;
pub use crossings::Crossing;
