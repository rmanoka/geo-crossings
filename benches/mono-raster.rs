#[path = "utils/random.rs"]
mod random;

use std::f64::consts::PI;

use criterion::{measurement::Measurement, *};
use geo::{
    prelude::{Area, BoundingRect},
    Polygon, map_coords::MapCoords, Coordinate, rotate::RotatePoint,
};
use geo_rasterize::BinaryBuilder;
use rand_distr::Standard;
use random::Samples;
use geo_crossings::monotone_chains;
use rand::{thread_rng, Rng};
use ndarray::Array2;

fn our_rasterize(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let mut arr = Array2::from_elem((height, width), false);

    let poly = poly.map_coords(|&(x, y)| (y, x));
    let (width, height) = (height, width);

    let bbox = poly.bounding_rect().unwrap();

    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let min_y = bbox.min().y;
    let max_y = bbox.max().y;
    let slice_height = (max_y - min_y) / height as f64;

    let cross_idx = |y: f64| {
        let j: f64 = (y - min_y) / slice_height;
        let j = j.max(0.).min(height as f64);
        let j = j.floor() as usize;
        j.min(height - 1)
    };

    monotone_chains(&poly)
        .into_iter()
        .for_each(|mono| {
            let mut scanner = mono.scan_lines(left);

            (0..width)
                .for_each(|i| {
                    let right = left + slice_width * i as f64;
                    let mut col_min_y = f64::INFINITY;
                    let mut col_max_y = -f64::INFINITY;
                    for trapz in scanner.advance_to(right) {
                        col_max_y = col_max_y.max(trapz.left.top).max(trapz.right.top);
                        col_min_y = col_min_y.min(trapz.left.bot).min(trapz.right.bot);
                    }

                    col_max_y = col_max_y.min(max_y);
                    col_min_y = col_min_y.max(min_y);

                    if col_min_y < col_max_y {
                        // min_y <= col_min_y < col_max_y <= max_x
                        let j1 = cross_idx(col_min_y);
                        let j2 = cross_idx(col_max_y) + 1;
                        for j in j1..j2 {
                            arr[(i, j)] = true;
                        }
                    }

                });

        });

    arr
}

fn geo_intersects(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let min_y = bbox.min().y;
    let max_y = bbox.max().y;
    let slice_height = (max_y - min_y) / height as f64;

    let poly = poly.map_coords(|&(x, y)| {
        let px = (x - left) / slice_width;
        let py = (y - min_y) / slice_height;
        (px, py)
    });

    let mut arr = Array2::from_elem((height, width), false);
    for j in 0..height {
        for i in 0..width {
            let coord = Coordinate {
                x: i as f64 + 0.5,
                y: j as f64 + 0.5,
            };

            use geo::algorithm::intersects::Intersects;
            if poly.intersects(&coord) {
                arr[(j, i)] = true;
            }
        }
    }
    arr
}

fn geo_rasterize(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let min_y = bbox.min().y;
    let max_y = bbox.max().y;
    let slice_height = (max_y - min_y) / height as f64;

    let poly = poly.map_coords(|&(x, y)| {
        let px = (x - left) / slice_width;
        let py = (y - min_y) / slice_height;
        (px, py)
    });

    let mut r = BinaryBuilder::new().width(width).height(height).build().unwrap();
    r.rasterize(&poly).unwrap();
    let pixels = r.finish();
    pixels
}

fn run_complex<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    const NUM_SLICES: usize = 4096;

    let mut group = c.benchmark_group("Zig-zag polygon rasterize");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (3..12).for_each(|scale| {
        let steps = 1 << scale;
        let polys = Samples::from_fn(SAMPLE_SIZE, || {
            let poly = random::simple_polygon(thread_rng(), steps);
            let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
            let poly = poly.rotate_around_point(angle, poly.exterior().0[0].into());
            let area = poly.unsigned_area();
            (poly, area)
        });

        group.bench_with_input(BenchmarkId::new("geo_rasterize", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    geo_rasterize(poly, NUM_SLICES, NUM_SLICES);
                },
                BatchSize::SmallInput,
            );
        });

        group.bench_with_input(BenchmarkId::new("montone_rasterize", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    our_rasterize(poly, NUM_SLICES, NUM_SLICES);
                },
                BatchSize::SmallInput,
            );
        });
    });
}

criterion_group!(complex, run_complex);

criterion_main!(complex);
