use std::iter::FromIterator;

use criterion::*;
use geo::{Rect, line_intersection::line_intersection};

const BBOX: [f64; 2] = [1024., 1024.];

#[path = "utils/random.rs"]
mod random;
use geo_crossings::Intersections;
use rand::thread_rng;
use random::*;

fn length_lc(c: &mut Criterion) {
    const NUM_LINES: usize = 1024;

    let bbox: Rect<f64> = Rect::new([0., 0.], BBOX);
    let line_len = BBOX[0] / 5.;

    let lines: Vec<_> = (0..NUM_LINES).map(|_| uniform_line_with_length(&mut thread_rng(), bbox, line_len)).collect();
    c.bench_function("Bentley-Ottman - short random lines", |b| {
        b.iter(|| {
            Intersections::from_iter(lines.iter()).count();
        })
    });
    c.bench_function("Brute-Force - short random lines", |b| {
        b.iter(|| {
            for l1 in lines.iter() {
                for l2 in lines.iter() {
                    black_box(
                        line_intersection(*l1, *l2)
                    );
                }
            }
        })
    });
}

fn uniform_lc(c: &mut Criterion) {
    const NUM_LINES: usize = 1024;
    let bbox: Rect<f64> = Rect::new([0., 0.], BBOX);

    let lines: Vec<_> = (0..NUM_LINES).map(|_| uniform_line(&mut thread_rng(), bbox)).collect();
    c.bench_function("Bentley-Ottman - uniform random lines", |b| {
        b.iter(|| {
            Intersections::from_iter(lines.iter()).count();
        })
    });
    c.bench_function("Brute-Force - uniform random lines", |b| {
        b.iter(|| {
            for l1 in lines.iter() {
                for l2 in lines.iter() {
                    black_box(
                        line_intersection(*l1, *l2)
                    );
                }
            }
        })
    });
}

criterion_group!(random, uniform_lc, length_lc);
criterion_main!(random);
