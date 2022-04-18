#![allow(unused)]
use std::f64::consts::PI;

use geo::{map_coords::MapCoords, rotate::RotatePoint, Coordinate, Line, Rect, Polygon, LineString};

use rand::{thread_rng, Rng};
use rand_distr::{Standard, Normal, Distribution};

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

pub fn simple_polygon<R: Rng>(mut rng: R, steps: usize) -> Polygon<f64> {
    let mut ring = Vec::with_capacity(2 * steps);

    let ystep = 1.0;
    let nudge_std = ystep / 1000.0;
    let mut y = 0.0;
    let normal = Normal::new(0.0, nudge_std * nudge_std).unwrap();

    ring.push((0.0, 0.0).into());
    (0..steps).for_each(|_| {
        let x: f64 = rng.sample(Standard);
        let x = (x * 10.) as i64 as f64 / 10.;
        y += ystep;
        // y += normal.sample(&mut rng);
        ring.push((x, y).into());
    });
    let shift = 50.0;
    ring.push((shift, y).into());
    (0..steps).for_each(|_| {
        let x: f64 = rng.sample(Standard);
        let x = (x * 10.) as i64 as f64 / 10.;
        y -= ystep;
        // y += normal.sample(&mut rng);
        ring.push((shift + x, y).into());
    });

    Polygon::new(LineString(ring), vec![])
}

pub struct Samples<T>(Vec<T>);
impl<T> Samples<T> {
    pub fn sampler<'a>(&'a self) -> impl FnMut() -> &'a T {
        let mut curr = 0;
        move || {
            let ret = curr;
            curr += 1;
            curr %= self.0.len();
            &self.0[ret]
        }
    }
    pub fn from_fn<F: FnMut() -> T>(size: usize, mut proc: F) -> Self {
        Self((0..size).map(|_| proc()).collect())
    }
}
