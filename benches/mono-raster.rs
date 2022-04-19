#[path = "utils/random.rs"]
mod random;

use std::f64::consts::PI;

use criterion::{measurement::Measurement, *};
use geo::{
    map_coords::MapCoords,
    prelude::{Area, BoundingRect},
    rotate::RotatePoint,
    Coordinate, Polygon,
};
use geo_crossings::monotone_chains;
use geo_rasterize::BinaryBuilder;
use ndarray::{Array2, s};
use rand::{thread_rng, Rng};
use rand_distr::Standard;
use random::Samples;

fn our_rasterize(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let mut arr = Array2::from_elem((height, width), false);
    let poly = poly.map_coords(|&(x, y)| (y, x));
    let (width, height) = (height, width);

    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let cross_min = bbox.min().y;
    let cross_max = bbox.max().y;

    monotone_chains(&poly).into_iter().for_each(|mono| {
        let mut scanner = mono.scan_lines(left);

        (0..width).for_each(|i| {
            let right = left + slice_width * i as f64;
            let (jmin, jmax) = scanner.cross_bounds(right, cross_min, cross_max, height);
            for j in jmin..=jmax {
                arr[(i, j)] = true;
            }
        });
    });
    arr
}

fn our_area(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<f64> {
    let mut arr = Array2::from_elem((height, width), 0.);
    let poly = poly.map_coords(|&(x, y)| (y, x));
    let (width, _height) = (height, width);

    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let cross_min = bbox.min().y;
    let cross_max = bbox.max().y;

    monotone_chains(&poly).into_iter().for_each(|mono| {
        let mut scanner = mono.scan_lines(left);

        (0..width).for_each(|i| {
            let right = left + slice_width * i as f64;
            let mut arr_slice = arr.slice_mut(s![i, ..]);
            let slice: &mut [f64] = arr_slice.as_slice_mut().unwrap();
            scanner.cross_area(right, cross_min, cross_max, slice);
        });
    });
    arr
}

#[allow(dead_code)]
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

    let mut r = BinaryBuilder::new()
        .width(width)
        .height(height)
        .build()
        .unwrap();
    r.rasterize(&poly).unwrap();

    r.finish()
}

fn run_complex<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    const NUM_SLICES: usize = 4096;

    let mut group = c.benchmark_group("Zig-zag polygon rasterize");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (3..12).for_each(|scale| {
        let steps = 1 << scale;
        let polys = Samples::from_fn(SAMPLE_SIZE, || {
            let poly = random::steppy_polygon(thread_rng(), steps);

            let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
            // 90 degrees is the worst inputs for our algo.
            // let angle: f64 = PI / 2.0;
            let poly = poly.rotate_around_point(angle, poly.exterior().0[0].into());
            let area = poly.unsigned_area();
            (poly, area)
        });
        group.sample_size(50);
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

        group.sample_size(10);
        group.bench_with_input(BenchmarkId::new("montone_rasterize_areas", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    our_area(poly, NUM_SLICES, NUM_SLICES);
                },
                BatchSize::SmallInput,
            );
        });
    });
}

fn run_convex<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    const NUM_SLICES: usize = 4096;

    let mut group = c.benchmark_group("Convex polygon rasterize");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (3..12).for_each(|scale| {
        let steps = 1 << scale;
        let polys = Samples::from_fn(SAMPLE_SIZE, || {
            let poly = random::convex_polygon(thread_rng(), steps);

            let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
            // 90 degrees is the worst inputs for our algo.
            // let angle: f64 = PI / 2.0;
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

        group.bench_with_input(BenchmarkId::new("montone_rasterize_areas", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    our_area(poly, NUM_SLICES, NUM_SLICES);
                },
                BatchSize::SmallInput,
            );
        });
    });
}

fn run_complex_vs_sample_size<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    const NUM_POLY_STEPS: usize = 1024;

    let mut group = c.benchmark_group("Zig-zag polygon rasterize vs samples");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    let polys = Samples::from_fn(SAMPLE_SIZE, || {
        let poly = random::steppy_polygon(thread_rng(), NUM_POLY_STEPS);

        let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
        // 90 degrees is the worst inputs for our algo.
        // let angle: f64 = PI / 2.0;
        let poly = poly.rotate_around_point(angle, poly.exterior().0[0].into());
        let area = poly.unsigned_area();
        (poly, area)
    });

    (3..12).for_each(|scale| {
        let steps = 1 << scale;

        group.bench_with_input(BenchmarkId::new("geo_rasterize", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    geo_rasterize(poly, steps, steps);
                },
                BatchSize::SmallInput,
            );
        });

        group.bench_with_input(BenchmarkId::new("montone_rasterize", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    our_rasterize(poly, steps, steps);
                },
                BatchSize::SmallInput,
            );
        });

        group.bench_with_input(BenchmarkId::new("montone_rasterize_areas", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    our_area(poly, steps, steps);
                },
                BatchSize::SmallInput,
            );
        });
    });
}


criterion_group!(verts_vs_time, run_complex, run_convex);
criterion_group!(samples_vs_time, run_complex_vs_sample_size);
criterion_main!(verts_vs_time, samples_vs_time);
