use geo::{winding_order::WindingOrder, Line};
use std::{cell::Cell, fmt::Display};

mod op;
pub use op::{Op, OpType};

mod rings;
use rings::*;

mod laminar;
pub use laminar::*;

#[cfg(test)]
mod tests {
    #![allow(unused)]
    use std::error::Error;

    use crate::crossings::tests::init_log;
    use crate::random::*;
    use geo::{Polygon, Rect};
    use log::info;
    use rand::thread_rng;
    use wkt::{ToWkt, TryFromWkt};

    use super::*;

    #[test]
    fn test_random() -> Result<(), Box<dyn Error>> {
        let p1 = circular_polygon(thread_rng(), 64);
        let p2 = circular_polygon(thread_rng(), 1024);
        check_sweep(&p1.wkt_string(), &p2.wkt_string(), OpType::Intersection)?;
        Ok(())
    }

    #[test]
    fn test_rect_overlapping() -> Result<(), Box<dyn Error>> {
        // Two rects that overlap
        let wkt1 = "POLYGON((0 0,1 0,1 1,0 1,0 0))";
        let wkt2 = "POLYGON((0.5 1,2 1,2 2,0.5 2,0.5 1))";
        check_sweep(wkt1, wkt2, OpType::Union)?;
        Ok(())
    }

    #[test]
    fn test_ext_in_hole() -> Result<(), Box<dyn Error>> {
        // A union which outputs a ring inside a hole inside a ext.
        let wkt1 = "POLYGON((0 0, 40 0, 40 40, 0 40, 0 0), (10 10, 30 10, 30 30, 10 30, 10 10))";
        let wkt2 = "POLYGON((11 11, 29 11, 29 29, 11 29, 11 11), (15 15, 25 15, 25 25, 15 25, 15 15))";
        check_sweep(wkt1, wkt2, OpType::Union)?;
        Ok(())
    }

    #[test]
    fn test_invalid_simple() -> Result<(), Box<dyn Error>> {
        // Polygon with holes and invalid
        let wkt1 = "POLYGON((0 0, 2 2, 2 0, 0 0), (1 1, 2 1, 1 0))";
        let wkt2 = "POLYGON EMPTY";
        check_sweep(wkt1, wkt2, OpType::Union)?;
        Ok(())
    }

    #[test]
    fn test_invalid_loops() -> Result<(), Box<dyn Error>> {
        let wkt1 = "POLYGON((0 0, 2 2, 0 4, -2 2, 0 0, 1 2, 0 3, -1 2, 0 0))";
        let wkt2 = "POLYGON EMPTY";
        check_sweep(wkt1, wkt2, OpType::Union)?;
        Ok(())

    }


    fn check_sweep(wkt1: &str, wkt2: &str, ty: OpType) -> Result<(), Box<dyn Error>> {
        init_log();
        let poly1 = Polygon::<f64>::try_from_wkt_str(wkt1)?;
        let poly2 = Polygon::try_from_wkt_str(wkt2)?;
        let bop = Op::new(&poly1, &poly2, ty);
        let rings = bop.sweep();
        info!("Got {n} rings", n = rings.len());
        for ring in rings.iter() {
            info!("\t{hole}: {wkt}", wkt = ring.coords().to_wkt(), hole = if ring.is_hole() { "HOLE" } else { "EXTR" });
        }

        let polygons = assemble(rings);
        info!("got {n} output polygons", n = polygons.len());
        for p in polygons {
            info!("\t{wkt}", wkt = p.to_wkt());
        }
        Ok(())
    }
}
