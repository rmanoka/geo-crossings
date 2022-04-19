mod segment;
mod sweep;

use segment::{Chain, Link, Segment};
pub use sweep::Sweep;

mod chains;
pub use chains::monotone_chains;

mod mono_poly;
pub use mono_poly::MonoPoly;

use geo::{kernels::Orientation, winding_order::WindingOrder, GeoFloat, GeoNum};

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Trip<T: GeoNum> {
    pub x: T,
    pub top: T,
    pub bot: T,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Trapz<T: GeoNum> {
    pub left: Trip<T>,
    pub right: Trip<T>,
}

impl<T: GeoNum> From<(Trip<T>, Trip<T>)> for Trapz<T> {
    fn from(tup: (Trip<T>, Trip<T>)) -> Self {
        Trapz {
            left: tup.0,
            right: tup.1,
        }
    }
}

impl<T: GeoFloat> Trapz<T> {
    #[inline]
    pub fn area(&self) -> T {
        let t1 = &self.left;
        let t2 = &self.right;

        let a = t1.top - t1.bot;
        let b = t2.top - t2.bot;
        debug_assert!(a >= T::zero() && b >= T::zero());
        let h = t2.x - t1.x;
        let two = T::one() + T::one();
        (a + b) / two * h
    }
}

#[cfg(test)]
mod tests {
    use approx::assert_relative_eq;
    use geo::{
        map_coords::MapCoords,
        prelude::{Area, BoundingRect},
        Polygon, *,
    };
    use log::info;
    use rand::thread_rng;

    use crate::{crossings::tests::init_log, monotone::MonoPoly};
    use crate::monotone_chains;
    use crate::random::{steppy_polygon, convex_polygon};

    #[test]
    fn check_ops_scanner() {
        init_log();
        verify_scanner_area(&poly_simple_merge());
        verify_scanner_area(&poly_simple_split());
        verify_scanner_area(&steppy_polygon(thread_rng(), 20));
        verify_scanner_area(&convex_polygon(thread_rng(), 20));
    }

    fn verify_scanner_area(poly: &Polygon<f64>) {
        let bbox = poly
            .bounding_rect()
            .expect("check_ops_scanner: bounding_rect");

        let left = bbox.min().x;
        let bot = bbox.min().y;
        let right = bbox.max().x;
        let top = bbox.max().y;

        const NUM_SLICES: usize = 16;
        let slice_width = (right - left) / NUM_SLICES as f64;

        let true_area = poly.unsigned_area();

        let total_area: f64 = monotone_chains(poly)
            .into_iter()
            .map(|mono: MonoPoly<f64>| -> f64 {
                info!("processing mono-polygon: {mono:?}");
                let mut scanner = mono.scan_lines(left);
                let mut vert_areas: [f64; NUM_SLICES] = [0.; NUM_SLICES];

                (0..NUM_SLICES)
                    .for_each(|i| {
                        let right = right.min(left + (i + 1) as f64 * slice_width);
                        scanner.cross_area(right, bot, top, &mut vert_areas);
                    });
                let total_area: f64 = vert_areas.iter().sum();
                info!("{total_area} vs {true_area}", true_area = mono.into_polygon().unsigned_area());

                total_area
            }).sum();
        info!("{total_area} vs {true_area}");
        assert_relative_eq!(total_area, true_area, epsilon = 1.0e-5);
    }

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
// TODO: AREA check
// INFO - processing mono-polygon: MonoPoly { top: [Pt(-2.0, 0.0), Pt(-0.0, 2.0), Pt(1.0, 1.0)], bot: [Pt(-2.0, 0.0), Pt(-0.2, 0.1), Pt(-0.2, 0.2), Pt(-0.1, 0.2), Pt(-0.0, -2.0), Pt(-0.0, 0.0), Pt(1.0, 1.0)] }
// INFO - 2.8899999999999997 vs 2.98
