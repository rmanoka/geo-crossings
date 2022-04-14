use std::cmp::Ordering;

use geo::{
    coords_iter::CoordsIter,
    winding_order::{Winding, WindingOrder},
    Coordinate, GeoNum, LineString,
};

use crate::SweepPoint;

pub struct ClosedRing<'a, T: GeoNum> {
    coords: &'a [Coordinate<T>],
    idx: usize,
    inverted: bool,
}

impl<'a, T: GeoNum> ClosedRing<'a, T> {
    pub fn new(mut ring: &'a LineString<T>) -> Self {
        let num_coords = ring.coords_count();
        let is_closed = ring.is_closed();
        assert!(num_coords > 2 + if is_closed { 1 } else { 0 });

        let coords = if is_closed { &ring.0[1..] } else { &ring.0[..] };

        let idx = coords
            .iter()
            .enumerate()
            .min_by(|c1, c2| SweepPoint::from(*c1.1).cmp(&SweepPoint::from(*c2.1)))
            .unwrap()
            .0;

        let orientation = ring.winding_order();
        assert!(orientation.is_some());

        // We want the standard ccw. iteration from idx
        let inverted = orientation == Some(WindingOrder::Clockwise);

        Self {
            coords,
            idx,
            inverted,
        }
    }

    pub fn increment(&mut self) {
        if self.inverted {
            self.idx = self.dec(self.idx);
        } else {
            self.idx = self.inc(self.idx);
        }
    }
    pub fn decrement(&mut self) {
        if self.inverted {
            self.idx = self.inc(self.idx);
        } else {
            self.idx = self.dec(self.idx);
        }
    }

    pub fn point(&self) -> Coordinate<T> {
        self.coords[self.idx]
    }

    pub fn prev_point(&self) -> Coordinate<T> {
        self.coords[dec(self.idx, self.coords.len())]
    }

    pub fn prev_segment(&self) -> [Coordinate<T>; 2] {
        todo!()
    }

    pub fn next_segment(&self) -> [Coordinate<T>; 2] {
        todo!()
    }
}

impl<'a, T: GeoNum> ClosedRing<'a, T> {
    fn inc(&self, idx: usize) -> usize {
        inc(idx, self.coords.len())
    }
    fn dec(&self, idx: usize) -> usize {
        dec(idx, self.coords.len())
    }
}

fn inc(mut x: usize, len: usize) -> usize {
    x += 1;
    if x >= len {
        x = 0;
    }
    x
}

fn dec(mut x: usize, len: usize) -> usize {
    if x == 0 {
        x = len - 1;
    } else {
        x -= 1;
    }
    x
}

// impl<T: GeoNum> From<LineString<T>> for ClosedRing<T> {
//     fn from(mut ls: LineString<T>) -> Self {
//         let num_coords = ls.coords_count();
//         assert!(num_coords > 0);
//         let coords = {
//             if num_coords == 1 {
//                 ls.0
//             } else if ls.is_closed() {
//                 let mut data = ls.0;
//                 data.pop().unwrap();
//                 data
//             } else {
//                 ls.0
//             }
//         };
//         todo!()
//     }
// }
