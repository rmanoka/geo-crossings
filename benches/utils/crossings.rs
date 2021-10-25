#![allow(dead_code)]

use std::iter::FromIterator;

use geo::{Line, line_intersection::line_intersection};
use geo_crossings::Intersections;
use rstar::{RTree, RTreeObject};

struct GeomWithData<R: RTreeObject, T>(R, T);

impl<R: RTreeObject, T> RTreeObject for GeomWithData<R, T> {
    type Envelope = R::Envelope;

    fn envelope(&self) -> Self::Envelope {
        self.0.envelope()
    }
}

pub fn count_bo(lines: &Vec<Line<f64>>) -> usize {
    Intersections::from_iter(lines.iter()).count()
}

pub fn count_brute(lines: &Vec<Line<f64>>) -> usize {
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

pub fn count_rtree(lines: &Vec<Line<f64>>) -> usize {
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
