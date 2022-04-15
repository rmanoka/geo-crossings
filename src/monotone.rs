mod segment;
mod sweep;
mod chains;

use segment::{Segment, Link};
pub use sweep::Sweep;

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
