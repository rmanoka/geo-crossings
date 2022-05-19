use geo::{winding_order::WindingOrder, GeoFloat, Line, Polygon, MultiPolygon, GeoNum};
use std::{cell::Cell, fmt::Display};

pub trait BooleanOp: Sized {
    type Scalar: GeoNum;

    fn intersection(&self, other: &Self) -> MultiPolygon<Self::Scalar>;
    fn union(&self, other: &Self) -> MultiPolygon<Self::Scalar>;
}

impl<T: GeoFloat> BooleanOp for Polygon<T> {
    type Scalar = T;

    fn intersection(&self, other: &Self) -> MultiPolygon<Self::Scalar> {
        let bop = Op::new(self, other, OpType::Intersection);
        let rings = bop.sweep();
        assemble(rings).into()
    }

    fn union(&self, other: &Self) -> MultiPolygon<Self::Scalar> {
        let bop = Op::new(self, other, OpType::Union);
        let rings = bop.sweep();
        assemble(rings).into()
    }
}

mod op;
use op::*;

mod rings;
use rings::*;

mod laminar;
use laminar::*;

#[cfg(test)]
mod tests {
    #![allow(unused)]
    use anyhow::{Context, Result, bail};
    use geo::{Geometry, MultiPolygon, Polygon, Rect};
    use geojson::{Feature, GeoJson};
    use log::info;
    use rand::thread_rng;
    use serde_derive::Serialize;
    use std::{
        convert::{TryFrom, TryInto},
        error::Error,
        fs::read_to_string,
    };
    use wkt::{ToWkt, TryFromWkt};

    use crate::{crossings::tests::init_log, random::*};

    use super::*;

    #[test]
    fn test_rect_overlapping() -> Result<()> {
        // Two rects that overlap
        let wkt1 = "POLYGON((0 0,1 0,1 1,0 1,0 0))";
        let wkt2 = "POLYGON((0.5 1,2 1,2 2,0.5 2,0.5 1))";

        let wkt_union = "MULTIPOLYGON(((2 1,1 1,1 0,0 0,0 1,0.5 1,0.5 2,2 2,2 1)))";
        let output = check_sweep(wkt1, wkt2, OpType::Union)?;
        assert_eq!(output, MultiPolygon::try_from_wkt_str(wkt_union).unwrap());
        Ok(())
    }

    #[test]
    fn test_ext_in_hole() -> Result<(), Box<dyn Error>> {
        // A union which outputs a ring inside a hole inside a ext.
        let wkt1 = "POLYGON((0 0, 40 0, 40 40, 0 40, 0 0), (10 10, 30 10, 30 30, 10 30, 10 10))";
        let wkt2 =
            "POLYGON((11 11, 29 11, 29 29, 11 29, 11 11), (15 15, 25 15, 25 25, 15 25, 15 15))";
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

    fn check_sweep(wkt1: &str, wkt2: &str, ty: OpType) -> Result<MultiPolygon<f64>> {
        init_log();
        let poly1 = Polygon::<f64>::try_from_wkt_str(wkt1).unwrap();
        let poly2 = Polygon::try_from_wkt_str(wkt2).unwrap();
        let bop = Op::new(&poly1, &poly2, ty);
        let rings = bop.sweep();
        info!("Got {n} rings", n = rings.len());
        for ring in rings.iter() {
            info!(
                "\t{hole}: {wkt}",
                wkt = ring.coords().to_wkt(),
                hole = if ring.is_hole() { "HOLE" } else { "EXTR" }
            );
        }

        let polygons = assemble(rings);
        info!("got {n} output polygons", n = polygons.len());
        for p in polygons.iter() {
            info!("\t{wkt}", wkt = p.to_wkt());
        }
        Ok(MultiPolygon::new(polygons))
    }

    #[test]
    fn generate_ds() -> Result<(), Box<dyn Error>> {
        init_log();
        for fix in vec![
            "fixtures/fatal1.geojson",
            "fixtures/fatal2.geojson",
            "fixtures/generic_test_cases/basic1_poly.geojson",
            "fixtures/generic_test_cases/basic2_poly_with_hole.geojson",
            "fixtures/generic_test_cases/basic3_multi_poly.geojson",
            "fixtures/generic_test_cases/basic4_multi_poly_with_hole.geojson",
            "fixtures/generic_test_cases/checkerboard1.geojson",
            "fixtures/generic_test_cases/closed_loop1.geojson",
            "fixtures/generic_test_cases/collinear_segments1.geojson",
            "fixtures/generic_test_cases/daef_cross_selfintersecting.geojson",
            "fixtures/generic_test_cases/daef_holed_rectangle2.geojson",
            "fixtures/generic_test_cases/daef_polygonwithholes_holed.geojson",
            "fixtures/generic_test_cases/disjoint_boxes.geojson",
            "fixtures/generic_test_cases/fatal1.geojson",
            "fixtures/generic_test_cases/fatal2.geojson",
            // "fixtures/generic_test_cases/fatal3.geojson",
            "fixtures/generic_test_cases/fatal4.geojson",
            "fixtures/generic_test_cases/filling_rectangle.geojson",
            "fixtures/generic_test_cases/hourglasses.geojson",
            "fixtures/generic_test_cases/intersections_at_endpoints.geojson",
            "fixtures/generic_test_cases/issue103.geojson",
            "fixtures/generic_test_cases/issue110.geojson",
            "fixtures/generic_test_cases/issue68.geojson",
            "fixtures/generic_test_cases/issue69.geojson",
            "fixtures/generic_test_cases/issue69_sub1.geojson",
            "fixtures/generic_test_cases/issue71.geojson",
            // "fixtures/generic_test_cases/issue76.geojson",
            "fixtures/generic_test_cases/issue93.geojson",
            "fixtures/generic_test_cases/issue96.geojson",
            "fixtures/generic_test_cases/issue99.geojson",
            "fixtures/generic_test_cases/many_rects.geojson",
            "fixtures/generic_test_cases/nested_polys1.geojson",
            "fixtures/generic_test_cases/nested_polys2.geojson",
            "fixtures/generic_test_cases/nested_polys3.geojson",
            "fixtures/generic_test_cases/overlap_loop.geojson",
            "fixtures/generic_test_cases/overlapping_segments1.geojson",
            "fixtures/generic_test_cases/overlapping_segments2.geojson",
            "fixtures/generic_test_cases/overlapping_segments3.geojson",
            "fixtures/generic_test_cases/overlap_y.geojson",
            "fixtures/generic_test_cases/polygon_trapezoid_edge_overlap.geojson",
            "fixtures/generic_test_cases/rust_issue12.geojson",
            "fixtures/generic_test_cases/tie.geojson",
            "fixtures/generic_test_cases/touching_boxes.geojson",
            "fixtures/generic_test_cases/vertical_ulp_slopes1.geojson",
            "fixtures/generic_test_cases/vertical_ulp_slopes2.geojson",
            "fixtures/generic_test_cases/xor_holes1.geojson",
            "fixtures/generic_test_cases/xor_holes2.geojson",
            "fixtures/hourglasses.geojson",
            "fixtures/overlap_loop.geojson",
            "fixtures/overlap_y.geojson",
            "fixtures/polygon_trapezoid_edge_overlap.geojson",
            "fixtures/rectangles.geojson",
            "fixtures/touching_boxes.geojson",
            "fixtures/two_shapes.geojson",
            "fixtures/two_triangles.geojson",
        ] {
            if let Err(e) = try_run_fixture(fix) {
                info!("error running fixture: {fix}");
                info!("\t{e}");
            }

            #[derive(Serialize)]
            struct TestCase {
                p1: String,
                p2: String,
                op: String,
                expected: String,
                ours: String,
            }

            fn try_run_fixture(fix: &str) -> Result<()> {
                let data = read_to_string(fix)?;
                let gjson: GeoJson = data.parse()?;
                match gjson {
                    GeoJson::FeatureCollection(fc) => {
                        if fc.features.len() <= 2 {
                            bail!("no ops geoms. found");
                        }
                        info!("reading: {fix}");
                        let p1: Polygon<f64> = feature_as_geom(&fc.features[0])
                            .context("geom 1 was not a polygon")?;
                        let p2: Polygon<_> = feature_as_geom(&fc.features[1])
                            .context("geom 2 was not a polygon")?;
                        info!("p1: {wkt}", wkt = p1.to_wkt());
                        info!("p2: {wkt}", wkt = p2.to_wkt());
                        fc.features.into_iter().skip(2).try_for_each(|feat| -> Result<()> {
                            let p: Geometry<f64> = feature_as_geom(&feat)?;
                            let props = feat.properties.unwrap();
                            let ty = props["operation"].as_str()
                                .context("operation was not a string")?;
                            info!(
                                "op: {ty} {wkt}",
                                wkt = p.to_wkt(),
                            );

                            if ty == "intersection" {
                                let is = p1.intersection(&p2);
                                info!("ours: {wkt}", wkt = is.to_wkt());
                            } else if ty == "union" {
                                let un = p1.union(&p2);
                                info!("ours: {wkt}", wkt = un.to_wkt());

                            }

                            Ok(())
                        });
                        // for feat in fc.features {
                        //     let p1: Geometry<f64> = feat.geometry.unwrap().try_into()?;
                        //     let p
                        //     info!("{p1:?}");
                        // }
                    }
                    _ => unreachable!(),
                }
                Ok(())
            }
        }

        Ok(())
    }

    fn feature_as_geom<T, E>(feat: &Feature) -> Result<T>
    where
        T: TryFrom<Geometry<f64>, Error = E>,
        E: std::fmt::Debug + Error + Sync + Send + 'static,
    {
        let p: Geometry<f64> = feat
            .geometry
            .clone()
            .context("missing geometry in feature")?
            .try_into()
            .context("could not parse feature as geometry")?;
        Ok(p.try_into()?)
    }
}
