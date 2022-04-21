use geo::{
    map_coords::MapCoords,
    prelude::BoundingRect,
    Coordinate, Polygon, coordinate_position::CoordPos,
};
use geo_crossings::monotone_chains;
use geo_rasterize::BinaryBuilder;
use ndarray::{Array2, s};

pub fn our_rasterize(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let mut arr = vec![false; height * width];

    let poly = poly.map_coords(|&(x, y)| (y, x));
    let (width, height) = (height, width);

    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let cross_min = bbox.min().y;
    let cross_max = bbox.max().y;

    for mono in monotone_chains(&poly) {
        let mut scanner = mono.scan_lines(left);

        let mut arr_start = 0;
        for i in 0..width {
            let right = left + slice_width * i as f64;
            let (jmin, jmax, _, _) = if let Some(b) = scanner.cross_bounds(right, cross_min, cross_max, height) {
                b
            } else {
                continue;
            };

            let slice = &mut arr[arr_start..(arr_start + width)];
            for j in jmin..=jmax {
                slice[j] = true;
            }
            arr_start += width;
        }
    }
    Array2::from_shape_vec((width, height), arr).unwrap()
}

pub fn our_area(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<f64> {
    let mut arr = Array2::from_elem((height, width), 0.);
    let poly = poly.map_coords(|&(x, y)| (y, x));
    let (width, _height) = (height, width);

    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let cross_min = bbox.min().y;
    let cross_max = bbox.max().y;

    monotone_chains(&poly).into_iter().for_each(|mono| {
        let mut scanner = mono.scan_lines(left);

        (0..width).for_each(|i| {
            let right = left + slice_width * i as f64;
            let mut arr_slice = arr.slice_mut(s![i, ..]);
            let slice: &mut [f64] = arr_slice.as_slice_mut().unwrap();
            scanner.cross_area(right, cross_min, cross_max, slice);
        });
    });
    arr
}

pub fn geo_intersects(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let min_y = bbox.min().y;
    let max_y = bbox.max().y;
    let slice_height = (max_y - min_y) / height as f64;

    let poly = poly.map_coords(|&(x, y)| {
        let px = (x - left) / slice_width;
        let py = (y - min_y) / slice_height;
        (px, py)
    });

    let mut arr = Array2::from_elem((height, width), false);
    for j in 0..height {
        for i in 0..width {
            let coord = Coordinate {
                x: i as f64 + 0.5,
                y: j as f64 + 0.5,
            };

            use geo::algorithm::coordinate_position::CoordinatePosition;
            if poly.coordinate_position(&coord) != CoordPos::Outside {
                arr[(j, i)] = true;
            }
        }
    }
    arr
}

pub fn geo_rasterize(poly: &Polygon<f64>, width: usize, height: usize) -> Array2<bool> {
    let bbox = poly.bounding_rect().unwrap();
    let left = bbox.min().x;
    let right = bbox.max().x;
    let slice_width = (right - left) / width as f64;

    let min_y = bbox.min().y;
    let max_y = bbox.max().y;
    let slice_height = (max_y - min_y) / height as f64;

    let poly = poly.map_coords(|&(x, y)| {
        let px = (x - left) / slice_width;
        let py = (y - min_y) / slice_height;
        (px, py)
    });

    let mut r = BinaryBuilder::new()
        .width(width)
        .height(height)
        .build()
        .unwrap();
    r.rasterize(&poly).unwrap();

    r.finish()
}
