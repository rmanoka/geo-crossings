use std::{fmt::Display, iter::FromIterator};

use criterion::{measurement::Measurement, *};
use geo::{line_intersection::line_intersection, map_coords::MapCoords, Coordinate, Line, Rect};

const BBOX: Coordinate<f64> = Coordinate { x: 1024., y: 1024. };

#[path = "utils/random.rs"]
mod random;
use geo_crossings::Intersections;
use rand::thread_rng;
use random::*;
use rstar::{RTree, RTreeObject};

struct GeomWithData<R: RTreeObject, T>(R, T);

impl<R: RTreeObject, T> RTreeObject for GeomWithData<R, T> {
    type Envelope = R::Envelope;

    fn envelope(&self) -> Self::Envelope {
        self.0.envelope()
    }
}

fn count_bo(lines: &Vec<Line<f64>>) -> usize {
    Intersections::from_iter(lines.iter()).count()
}

fn count_brute(lines: &Vec<Line<f64>>) -> usize {
    let mut count = 0;
    let n = lines.len();
    for i in 0..n {
        let l1 = &lines[i];
        for j in i + 1..n {
            let l2 = &lines[j];
            if line_intersection(*l1, *l2).is_some() {
                count += 1;
            }
        }
    }
    count
}

fn count_rtree(lines: &Vec<Line<f64>>) -> usize {
    let lines: Vec<_> = lines
        .iter()
        .enumerate()
        .map(|(i, l)| GeomWithData(*l, i))
        .collect();

    let tree = RTree::bulk_load(lines);
    tree.intersection_candidates_with_other_tree(&tree)
        .filter_map(|(l1, l2)| {
            if l1.1 >= l2.1 {
                None
            } else {
                line_intersection(l1.0, l2.0)
            }
        })
        .count()
}

fn bench_algos<T, F, I>(g: &mut BenchmarkGroup<T>, mut gen: F, sample_size: usize, param: I)
where
    T: Measurement,
    F: FnMut() -> Vec<Line<f64>>,
    I: Display + Copy,
{
    let samples: Vec<_> = (0..sample_size)
        .map(|_| {
            let lines = gen();
            let expected = count_brute(&lines);
            (lines, expected)
        })
        .collect();

    let mut curr = 0;
    let mut iter_gen = || {
        let ret = curr;
        curr += 1;
        curr %= samples.len();
        &samples[ret]
    };

    (0..1).for_each(|_| {
        g.bench_with_input(BenchmarkId::new("Bentley-Ottman", param), &(), |b, _| {
            b.iter_batched(
                &mut iter_gen,
                |lines| {
                    assert_eq!(count_bo(&lines.0), lines.1);
                },
                BatchSize::SmallInput,
            );
        });
        g.bench_with_input(BenchmarkId::new("Brute-Force", param), &(), |b, _| {
            b.iter_batched(
                &mut iter_gen,
                |lines| {
                    assert_eq!(count_brute(&lines.0), lines.1);
                },
                BatchSize::SmallInput,
            );
        });
        g.bench_with_input(BenchmarkId::new("R-Tree", param), &(), |b, _| {
            b.iter_batched(
                &mut iter_gen,
                |lines| {
                    assert_eq!(count_rtree(&lines.0), lines.1);
                },
                BatchSize::SmallInput,
            );
        });
    });
}

fn short(c: &mut Criterion) {
    const NUM_LINES: usize = 4096;
    const SAMPLE_SIZE: usize = 16;

    let mut group = c.benchmark_group("Short lines");
    group.sample_size(2 * SAMPLE_SIZE);
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (0..10).for_each(|scale| {
        let scaling: f64 = (1 << scale) as f64;
        let bounds = Rect::new([0., 0.].into(), BBOX / scaling);
        let shift_bounds = Rect::new([0., 0.].into(), BBOX - (BBOX / scaling));

        bench_algos(
            &mut group,
            || {
                (0..NUM_LINES)
                    .map(|_| {
                        let shift = uniform_point(&mut thread_rng(), shift_bounds);
                        uniform_line(&mut thread_rng(), bounds)
                            .map_coords(|(x, y)| (x + shift.x, y + shift.y))
                    })
                    .collect()
            },
            SAMPLE_SIZE,
            1. / scaling,
        );
    });
}

fn uniform(c: &mut Criterion) {
    const SAMPLE_SIZE: usize = 16;

    let mut group = c.benchmark_group("Random lines");
    group.sample_size(2 * SAMPLE_SIZE);
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    let scale = 4;
    let scaling: f64 = (1 << scale) as f64;
    let bounds = Rect::new([0., 0.].into(), BBOX / scaling);
    let shift_bounds = Rect::new([0., 0.].into(), BBOX - (BBOX / scaling));

    (0..12).step_by(2).for_each(|log_num_lines| {
        let num_lines = 1 << log_num_lines;
        bench_algos(
            &mut group,
            || {
                (0..num_lines)
                    .map(|_| {
                        let shift = uniform_point(&mut thread_rng(), shift_bounds);
                        uniform_line(&mut thread_rng(), bounds)
                            .map_coords(|(x, y)| (x + shift.x, y + shift.y))
                    })
                    .collect()
            },
            SAMPLE_SIZE,
            num_lines,
        );
    });
}

fn mixed(c: &mut Criterion) {
    const SAMPLE_SIZE: usize = 16;

    let mut group = c.benchmark_group("Mixed");
    group.sample_size(2 * SAMPLE_SIZE);
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    (3..12).step_by(2).for_each(|log_num_lines| {
        let num_lines = 1 << log_num_lines;
        bench_algos(
            &mut group,
            || {
                (0..8)
                    .flat_map(|scale| {
                        let scaling: f64 = (1 << scale) as f64;
                        let bounds = Rect::new([0., 0.].into(), BBOX / scaling);
                        let shift_bounds = Rect::new([0., 0.].into(), BBOX - (BBOX / scaling));

                        (0..num_lines / 8).map(move |_| {
                            let shift = uniform_point(&mut thread_rng(), shift_bounds);
                            uniform_line(&mut thread_rng(), bounds)
                                .map_coords(|(x, y)| (x + shift.x, y + shift.y))
                        })
                    })
                    .collect()
            },
            SAMPLE_SIZE,
            num_lines,
        );
    });
}

criterion_group!(random, uniform, short, mixed);
criterion_main!(random);
