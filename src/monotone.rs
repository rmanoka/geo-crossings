mod segment;
mod sweep;
mod rasterize;

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
    use approx::{assert_relative_eq, assert_abs_diff_eq};
    use geo::{
        map_coords::MapCoords,
        prelude::{Area, BoundingRect},
        Polygon, *,
    };
    use log::{info, debug};
    use rand::{thread_rng, Rng};
    use rand_distr::Standard;

    use crate::{crossings::tests::init_log, monotone::MonoPoly};
    use crate::monotone_chains;
    use crate::random::{steppy_polygon, circular_polygon};

    #[test]
    fn check_scanner_bounds() {
        init_log();
        verify_scanner_bounds(&poly_simple_merge());
        verify_scanner_bounds(&poly_simple_split());
        verify_scanner_bounds(&steppy_polygon(thread_rng(), 20));
        verify_scanner_bounds(&circular_polygon(thread_rng(), 20));
    }

    #[test]
    fn check_scanner_area() {
        init_log();
        verify_scanner_area(&poly_simple_merge());
        verify_scanner_area(&poly_simple_split());
        verify_scanner_area(&steppy_polygon(thread_rng(), 20));
        verify_scanner_area(&circular_polygon(thread_rng(), 20));
    }

    fn verify_scanner_bounds(poly: &Polygon<f64>) {
        let bbox = poly
            .bounding_rect()
            .expect("check_ops_scanner: bounding_rect");

        let left = bbox.min().x - thread_rng().sample::<f64, _>(Standard);
        let bot = bbox.min().y - thread_rng().sample::<f64, _>(Standard);
        let right: f64 = bbox.max().x + thread_rng().sample::<f64, _>(Standard);
        let top = bbox.max().y + thread_rng().sample::<f64, _>(Standard);

        for num_slices in [16, 64, 128, 1024] {
            let slice_width = (right - left) / num_slices as f64;
            let slice_height = (top - bot) / num_slices as f64;
            let cell_area = slice_width * slice_height;

            let true_area = poly.unsigned_area();
            let (mut total_area_lb, mut total_area_ub) = (0.0_f64, 0.0_f64);
            let mpolys = monotone_chains(poly);
            let num_mploys = mpolys.len();
            mpolys
                .into_iter()
                .for_each(|mono: MonoPoly<f64>| {
                    info!("processing mono-polygon: {mono:?}");
                    let mut scanner = mono.scan_lines(left);
                    let mut area_lb = 0.;
                    let mut area_ub = 0.;

                    (0..num_slices)
                        .for_each(|i| {
                            let curr_left = scanner.curr().x;
                            let right = right.min(left + (i + 1) as f64 * slice_width);
                            if let Some((j_min, j_max, i_min, i_max)) = scanner.cross_bounds(right, bot, top, num_slices) {
                                let curr_right = scanner.curr().x.min(right);
                                let curr_ub = (1 + j_max - j_min) as f64 * cell_area;
                                area_ub += curr_ub;
                                if i_max > i_min {
                                    let curr_lb = (i_max - i_min) * (curr_right - curr_left);
                                    assert!(curr_lb <= curr_ub);
                                    area_lb += curr_lb;
                                }
                            }
                            // scanner.cross_area(right, bot, top, &mut vert_areas);
                        });
                    // let total_area: f64 = vert_areas.iter().sum();
                    let true_area = mono.into_polygon().unsigned_area();
                    debug!("{area_lb} <= {true_area} <= {area_ub}");
                    assert!(area_lb <= true_area);
                    assert!(area_ub >= true_area);

                    total_area_lb += area_lb;
                    total_area_ub += area_ub;

                });
            if num_slices < 1000 { continue; }

            let max_err = 2. * cell_area * num_slices as f64 * num_mploys as f64;

            info!("{total_area_lb} <= {true_area} <= {total_area_ub} (err <= {max_err})");
            assert_abs_diff_eq!(total_area_lb, total_area_ub, epsilon = max_err);
        }
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
