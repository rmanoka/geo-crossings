use geo::{coords_iter::CoordsIter, winding_order::Winding, GeoFloat, LineString, Polygon};
use log::{debug, trace};
use std::{cmp::Ordering, iter::FromIterator};

use super::*;
use crate::{
    utils::winding_inverse, Crossable, Crossing, CrossingsIter,
    LineOrPoint, Float,
};

#[derive(Debug, Clone)]
pub struct Op<T: Float> {
    ty: OpType,
    edges: Vec<Edge<T>>,
}

impl<T: Float> Op<T> {
    pub fn new(ty: OpType, capacity: usize) -> Self {
        Op {
            ty,
            edges: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn add_multi_polygon(&mut self, mp: &MultiPolygon<T>, is_first: bool) {
        mp.0.iter().for_each(|p| self.add_polygon(p, is_first));
    }

    pub(crate) fn add_polygon(&mut self, poly: &Polygon<T>, is_first: bool) {
        self.add_closed_ring(poly.exterior(), is_first, false);
        for hole in poly.interiors() {
            self.add_closed_ring(hole, is_first, true);
        }
    }
    fn add_closed_ring(&mut self, ring: &LineString<T>, is_first: bool, is_hole: bool) {
        assert!(ring.is_closed());
        if ring.coords_count() <= 3 { return; }

        let winding = {
            let winding = ring.winding_order().expect("ring has a winding order");
            if is_hole {
                winding_inverse(winding)
            } else {
                winding
            }
        };
        debug!("input winding: {winding:?}");
        for line in ring.lines() {
            let lp: LineOrPoint<_> = line.into();
            if !lp.is_line() {
                continue;
            }

            let winding = if lp.first().coord() == line.start {
                winding.clone()
            } else {
                winding_inverse(winding.clone())
            };
            debug!("processing: {lp:?} -> {winding:?}");

            let region = Region::infinity(self.ty);
            self.edges.push(Edge {
                geom: lp,
                is_first,
                region: region.into(),
                winding,
            });
        }
    }

    pub fn sweep(&self) -> Vec<Ring<T>> {
        let mut iter = CrossingsIter::from_iter(self.edges.iter());
        let mut rings = Rings::default();

        while let Some(pt) = iter.next() {
            trace!(
                "\n\nSweep point: {pt:?}, {n} intersection segments",
                n = iter.intersections_mut().len()
            );
            // Note: it is important to use a stable-sort
            // here as we want overlapping edges to be left
            // in the given ordering. This is needed for:
            //
            // (1) correctness of the `has_overlap` flag;
            //
            // (2) maintaining the position of the first
            // segment of a overlapping collection. This
            // segment is the only active-segment in the
            // b-tree maintained by the iterator, and thus
            // we need to ensure to update the region on
            // this segment (and not on any of the
            // equivalent overlapping segments).

            fn compare_crossings<X: Crossable>(a: &Crossing<X>, b: &Crossing<X>) -> Ordering {
                a.at_left.cmp(&b.at_left).then_with(|| {
                    let ord = LineOrPoint::from(a.line)
                        .partial_cmp(&b.line.into())
                        .unwrap();
                    if a.at_left {
                        ord
                    } else {
                        ord.reverse()
                    }
                })
            }
            iter.intersections_mut().sort_by(compare_crossings);

            // Process all end-segments.
            let mut idx = 0;
            let mut next_region = None;
            while idx < iter.intersections().len() {
                let c = &iter.intersections()[idx];
                // If we hit a start-segment, we are done.
                if c.at_left {
                    break;
                }
                let cross = c.crossable;
                if next_region.is_none() {
                    next_region = Some(cross.region.get());
                    trace!(
                        "get_region: {geom:?}: {next_region}",
                        next_region = next_region.unwrap(),
                        geom = LineOrPoint::from(c.line),
                    );
                }
                next_region.as_mut().unwrap().cross(cross.is_first);
                let has_overlap = (idx + 1) < iter.intersections().len()
                    && compare_crossings(c, &iter.intersections()[idx + 1]) == Ordering::Equal;
                if !has_overlap {
                    let prev_region = cross.region.get();
                    trace!(
                        "check_add: {geom:?}: {prev_region} -> {next_region}",
                        geom = LineOrPoint::from(c.line),
                        next_region = next_region.unwrap()
                    );
                    let next_is_ty = next_region.unwrap().is_ty(self.ty);
                    if prev_region.is_ty(self.ty) ^ next_is_ty {
                        rings.add_edge(
                            c.line,
                            if !next_is_ty {
                                WindingOrder::CounterClockwise
                            } else {
                                WindingOrder::Clockwise
                            },
                        )
                    }
                    next_region = None;
                }
                idx += 1;
            }

            if idx >= iter.intersections_mut().len() {
                continue;
            }
            let botmost_start_segment = iter.intersections_mut()[idx];
            debug_assert!(botmost_start_segment.at_left);

            trace!(
                "Bottom most start-edge: {botmost:?}",
                botmost = LineOrPoint::from(botmost_start_segment.line),
            );

            let prev = iter.prev_active(&botmost_start_segment);
            trace!(
                "prev-active(bot-most): {prev:?}",
                prev = prev.map(|p| p.geom)
            );

            let mut region = prev
                .as_ref()
                .map(|c| c.region.get())
                .unwrap_or_else(|| Region::infinity(self.ty));
            trace!("bot region: {region}");

            while idx < iter.intersections().len() {
                let mut c = &iter.intersections()[idx];
                let mut jdx = idx;
                loop {
                    region.cross(c.crossable.is_first);
                    let has_overlap = (idx + 1) < iter.intersections().len()
                        && compare_crossings(c, &iter.intersections()[idx + 1]) == Ordering::Equal;
                    if !has_overlap {
                        break;
                    }
                    idx += 1;
                    c = &iter.intersections()[idx];
                }
                trace!(
                    "set_region: {geom:?} => {region} ({c} counts)",
                    geom = LineOrPoint::from(iter.intersections_mut()[jdx].line),
                    c = idx - jdx + 1,
                );
                while jdx <= idx {
                    iter.intersections_mut()[jdx].crossable.region.set(region);
                    jdx += 1;
                }
                idx += 1;
            }
        }

        rings.finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpType {
    Intersection,
    Union,
    Difference,
    Xor,
}

#[derive(Debug, Clone, Copy)]
struct Region {
    is_first: bool,
    is_second: bool,
}
impl Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{f}{s}]",
            f = if self.is_first { "A" } else { "" },
            s = if self.is_second { "B" } else { "" },
        )
    }
}

impl Region {
    fn infinity(ty: OpType) -> Self {
        Region {
            is_first: false,
            is_second: matches!(ty, OpType::Difference),
        }
    }
    fn cross(&mut self, first: bool) {
        if first {
            self.is_first = !self.is_first;
        } else {
            self.is_second = !self.is_second;
        }
    }
    fn is_ty(&self, ty: OpType) -> bool {
        match ty {
            OpType::Intersection | OpType::Difference => self.is_first && self.is_second,
            OpType::Union => self.is_first || self.is_second,
            OpType::Xor => self.is_first ^ self.is_second,
        }
    }
}

#[derive(Clone)]
struct Edge<T: Float> {
    geom: LineOrPoint<T>,
    is_first: bool,
    region: Cell<Region>,
    winding: WindingOrder,
}

impl<T: Float> std::fmt::Debug for Edge<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line = self.geom.line();
        f.debug_struct("Edge")
            .field(
                "geom",
                &format!(
                    "({:?},{:?}) <-> ({:?},{:?})",
                    line.start.x, line.start.y, line.end.x, line.end.y
                ),
            )
            .field("is_first", &self.is_first)
            .field("region", &self.region)
            .field("winding", &self.winding)
            .finish()
    }
}

impl<T: Float> Crossable for Edge<T> {
    type Scalar = T;

    fn line(&self) -> Line<Self::Scalar> {
        self.geom.line()
    }
}
