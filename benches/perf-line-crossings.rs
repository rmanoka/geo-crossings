#[path = "utils/random.rs"]
mod random;

#[path = "utils/crossings.rs"]
mod crossings;

use crossings::*;
use geo::Coordinate;
use random::*;

const BBOX: Coordinate<f64> = Coordinate { x: 1024., y: 1024. };

fn uniform_perf() {
    const SAMPLE_SIZE: usize = 128;
    const SCALE: usize = 3;

    let line_gen = scaled_generator(BBOX, SCALE);

    (9..12).step_by(2).for_each(|log_num_lines| {
        let num_lines = 1 << log_num_lines;
        let lines: Vec<_> = (0..num_lines).map(|_| line_gen()).collect();
        eprintln!("Profiling with {} lines (scale = {})", num_lines, SCALE);

        (0..SAMPLE_SIZE).for_each(|_| {
            count_bo(&lines);
        });
    });
}

fn main() {
    uniform_perf();
}
