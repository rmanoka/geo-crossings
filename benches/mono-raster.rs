#[path = "utils/random.rs"]
mod random;
use random::Samples;

#[path = "utils/rasterize.rs"]
mod rasterize;
use rasterize::{our_area, our_rasterize, geo_intersects, geo_rasterize};

use std::f64::consts::PI;

use criterion::{measurement::Measurement, *};
use geo::{
    prelude::Area,
    rotate::RotatePoint,
};

use rand::{thread_rng, Rng};
use rand_distr::Standard;

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

        group.bench_with_input(BenchmarkId::new("geo_coord_pos", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, _)| {
                    geo_intersects(poly, NUM_SLICES, NUM_SLICES);
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
