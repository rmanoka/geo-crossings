#[path = "utils/random.rs"]
mod random;

use approx::assert_relative_eq;
use criterion::{measurement::Measurement, *};
use geo::{
    prelude::{Area, BoundingRect},
    Polygon,
};
use geo_crossings::monotone_chains;
use rand::thread_rng;

fn verify_scanner_area(poly: &Polygon<f64>, num_slices: usize) -> f64 {
    let bbox = poly
        .bounding_rect()
        .expect("check_ops_scanner: bounding_rect");

    let left = bbox.min().x;
    let right = bbox.max().x;

    let slice_width = (right - left) / num_slices as f64;

    let total_area: f64 = monotone_chains(poly)
        .into_iter()
        .map(|mono| -> f64 {
            let mut left = left;
            let mut scanner = mono.scan_lines(left);

            (0..num_slices)
                .map(|_| {
                    let right = (left + slice_width).min(right);

                    let mut area = 0.;
                    for trapz in scanner.advance_to(right) {
                        area += trapz.area();
                    }
                    left = right;
                    area
                })
                .sum()
        })
        .sum();
    total_area
}

fn run_complex<T: Measurement>(c: &mut Criterion<T>) {
    const SAMPLE_SIZE: usize = 16;
    const NUM_SLICES: usize = 4096;

    let mut group = c.benchmark_group("Complex polygon");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (3..12).for_each(|scale| {
        let steps = 1 << scale;
        let polys = Samples::from_fn(SAMPLE_SIZE, || {
            let poly = random::simple_polygon(thread_rng(), steps);
            let area = poly.unsigned_area();
            (poly, area)
        });

        group.bench_with_input(BenchmarkId::new("Ours", steps), &(), |b, _| {
            b.iter_batched(
                polys.sampler(),
                |&(ref poly, true_area)| {
                    let total_area = verify_scanner_area(poly, NUM_SLICES);
                    assert_relative_eq!(total_area, true_area, epsilon = 1.0e-6);
                },
                BatchSize::SmallInput,
            );
        });
    });
}

criterion_group!(complex, run_complex);

criterion_main!(complex);
