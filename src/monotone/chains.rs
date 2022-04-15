#![allow(unused)]
use geo::{winding_order::WindingOrder, GeoNum, LineString, coords_iter::CoordsIter};
use smallvec::SmallVec;

use crate::SweepPoint;

use super::Sweep;

const CHAIN_STACK_SIZE: usize = 16;
type Coords<T> = SmallVec<[SweepPoint<T>; CHAIN_STACK_SIZE]>;

struct Chain<T: GeoNum> {
    interior: WindingOrder,
    other: usize,
    coords: Coords<T>,
}


type Chains<T> = SmallVec<[Chain<T>; CHAIN_STACK_SIZE]>;

fn chains<T: GeoNum + Unpin>(ring: LineString<T>) {
    let num_coords = ring.coords_count();
    let chains = Chains::<T>::new();

    let sweep = Box::pin(Sweep::from_closed_ring(ring));
    for (pt, links) in sweep {
        for link in links {

        }
    }
}

#[cfg(test)]
mod tests {
    use geo::{map_coords::MapCoords, LineString, Coordinate};
    use crate::{crossings::tests::init_log, monotone::Sweep};

    #[test]
    fn check_sweep_iter() {
        init_log();

        let ring = LineString(vec![
            Coordinate::<f64>::from([0., 0.]),
            [-1., 1.].into(),
            [0., 2.].into(),
            [2., 0.].into(),
            [0., -2.].into(),
            [-1., -1.].into(),
        ]);
        let mirror: LineString<_> = ring.map_coords(|&(x, y)| (-x, y));
        let sweep = Box::pin(Sweep::from_closed_ring(ring));
        for event in sweep {
            eprintln!("{event:?}");
        }
        eprintln!("==================================== mirror =================================");
        let sweep = Box::pin(Sweep::from_closed_ring(mirror));
        for event in sweep {
            eprintln!("{event:?}");
        }
    }
}
