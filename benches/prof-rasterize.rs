#[path = "utils/random.rs"]
mod random;
use random::Samples;

#[path = "utils/rasterize.rs"]
mod rasterize;
use rasterize::*;

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

    let mut group = c.benchmark_group("profile monotone-rasterize with Zig-zag polygons");
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
        group.bench_with_input(BenchmarkId::new("montone_rasterize", steps), &(), |b, _| {
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

criterion_group!(profile, run_complex);
criterion_main!(profile);
