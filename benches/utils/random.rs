use std::f64::consts::PI;

use geo::{map_coords::MapCoords, rotate::RotatePoint, Coordinate, Line, Rect};

use rand::{thread_rng, Rng};
use rand_distr::Standard;

#[inline]
pub fn uniform_point<R: Rng>(rng: &mut R, bounds: Rect<f64>) -> Coordinate<f64> {
    let coords: [f64; 2] = rng.sample(Standard);
    let dims = bounds.max() - bounds.min();
    Coordinate {
        x: bounds.min().x + dims.x * coords[0],
        y: bounds.min().y + dims.y * coords[1],
    }
}

#[inline]
pub fn uniform_line<R: Rng>(rng: &mut R, bounds: Rect<f64>) -> Line<f64> {
    Line::new(uniform_point(rng, bounds), uniform_point(rng, bounds))
}

#[inline]
#[allow(dead_code)]
pub fn uniform_line_with_length<R: Rng>(rng: &mut R, bounds: Rect<f64>, length: f64) -> Line<f64> {
    let start = uniform_point(rng, bounds);
    let line = Line::new(start, start + (length, 0.).into());
    let angle = rng.sample::<f64, _>(Standard) * 2. * PI;
    line.rotate_around_point(angle, start.into())
}

pub fn scaled_generator(dims: Coordinate<f64>, scale: usize) -> impl Fn() -> Line<f64> {
    let scaling: f64 = (1 << scale) as f64;
    let bounds = Rect::new([0., 0.].into(), dims / scaling);
    let shift_bounds = Rect::new([0., 0.].into(), dims - (dims / scaling));

    move || {
        let shift = uniform_point(&mut thread_rng(), shift_bounds);
        uniform_line(&mut thread_rng(), bounds).map_coords(|(x, y)| (x + shift.x, y + shift.y))
    }
}
