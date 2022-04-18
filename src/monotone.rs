mod segment;
mod sweep;

use segment::{Link, Segment, Chain};
pub use sweep::Sweep;

mod chains;
pub use chains::monotone_chains;

mod ops;
pub use ops::{MonoPoly, Scanner};

use geo::{kernels::Orientation, winding_order::WindingOrder};

fn winding_order_from_orientation(ori: Orientation) -> Option<WindingOrder> {
    match ori {
        Orientation::CounterClockwise => Some(WindingOrder::CounterClockwise),
        Orientation::Clockwise => Some(WindingOrder::Clockwise),
        _ => None,
    }
}

#[allow(dead_code)]
fn winding_order_as_orientation(winding: &WindingOrder) -> Orientation {
    match winding {
        WindingOrder::Clockwise => Orientation::Clockwise,
        WindingOrder::CounterClockwise => Orientation::CounterClockwise,
    }
}

fn winding_inverse(winding: WindingOrder) -> WindingOrder {
    match winding {
        WindingOrder::Clockwise => WindingOrder::CounterClockwise,
        WindingOrder::CounterClockwise => WindingOrder::Clockwise,
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use geo::{*, map_coords::MapCoords};

    pub fn poly_simple_merge() -> Polygon<f64> {
        let ring = LineString(vec![
            Coordinate::<f64>::from([0., 0.]),
            [-1., 1.].into(),
            [0., 2.].into(),
            [2., 0.].into(),
            [0., -2.].into(),
            [-1., -1.].into(),
        ]);
        let hole = LineString(vec![
            Coordinate::<f64>::from([0.1, 0.1]),
            [0.1, 0.2].into(),
            [0.2, 0.2].into(),
            [0.2, 0.1].into(),
        ]);
        Polygon::new(ring, vec![hole])
    }

    pub fn poly_simple_split() -> Polygon<f64> {
        poly_simple_merge().map_coords(|&(x, y)| (-x, y))
    }

}
