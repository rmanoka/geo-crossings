mod segment;
mod sweep;

pub(crate) use segment::Segment;
pub use sweep::Sweep;

use crate::events::Event;

use geo::{kernels::Orientation, winding_order::WindingOrder, GeoNum};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VertexType {
    Start,
    Split,
    End,
    Merge,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Intersection<T: GeoNum> {
    ty: VertexType,
    event_1: Event<T>,
    event_2: Event<T>,
    orientation: Orientation,
    interior: WindingOrder,
}

fn winding_order_from_orientation(ori: Orientation) -> Option<WindingOrder> {
    match ori {
        Orientation::CounterClockwise => Some(WindingOrder::CounterClockwise),
        Orientation::Clockwise => Some(WindingOrder::Clockwise),
        _ => None,
    }
}

fn winding_inverse(winding: WindingOrder) -> WindingOrder {
    match winding {
        WindingOrder::Clockwise => WindingOrder::CounterClockwise,
        WindingOrder::CounterClockwise => WindingOrder::Clockwise,
    }
}
