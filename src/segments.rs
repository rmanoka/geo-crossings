use geo_types::{CoordFloat, Line};

/// Interface for types that can be processed to detect crossings.
pub trait Crossable {
    type Scalar: CoordFloat;
    fn geom(&self) -> Line<Self::Scalar>;
}
