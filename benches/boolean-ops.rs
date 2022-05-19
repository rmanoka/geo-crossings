use std::f64::consts::PI;

use criterion::{measurement::Measurement, *};
use geo::{
    rotate::RotatePoint, intersects::Intersects,
};

use geo_crossings::BooleanOp;
use rand::{thread_rng, Rng};
use rand_distr::Standard;

#[path = "utils/random.rs"]
mod random;
use random::Samples;

fn run_complex<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    let mut group = c.benchmark_group("Circular polygon boolean-ops");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (8..20).for_each(|scale| {
        let steps = 1 << scale;
        let polys = Samples::from_fn(SAMPLE_SIZE, || {
            let poly1 = random::steppy_polygon(thread_rng(), steps);

            let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
            // 90 degrees is the worst inputs for our algo.
            // let angle: f64 = PI / 2.0;
            let poly1 = poly1.rotate_around_point(angle, poly1.exterior().0[0].into());

            let poly2 = random::circular_polygon(thread_rng(), steps);

            let angle: f64 = thread_rng().sample::<f64, _>(Standard) * PI * 2.0;
            // 90 degrees is the worst inputs for our algo.
            // let angle: f64 = PI / 2.0;
            let poly2 = poly2.rotate_around_point(angle, poly2.exterior().0[0].into());
            (poly1, poly2)
        });

        group.sample_size(50);
        group.bench_with_input(BenchmarkId::new("bops::union", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, ref poly2)| {
                    poly.intersection(poly2)
                },
                BatchSize::SmallInput,
            );
        });

        group.bench_with_input(BenchmarkId::new("bops::intersection", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, ref poly2)| {
                    poly.union(poly2)
                },
                BatchSize::SmallInput,
            );
        });

        group.bench_with_input(BenchmarkId::new("geo::intersects", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, ref poly2)| {
                    poly.intersects(poly2)
                },
                BatchSize::SmallInput,
            );
        });
    });
}

criterion_group!(verts_vs_time, run_complex);
criterion_main!(verts_vs_time);
