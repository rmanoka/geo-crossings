mod segment;
mod sweep;

use segment::{Link, Segment, Chain};
pub use sweep::Sweep;

mod chains;
pub use chains::monotone_chains;

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
