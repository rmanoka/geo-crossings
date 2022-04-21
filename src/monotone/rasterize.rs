use geo::{GeoFloat, Coordinate, LineString};
use super::{mono_poly::Scanner, MonoPoly};

impl<'a, T: GeoFloat> Scanner<'a, T> {
    pub fn cross_ubounds(
        &mut self,
        right: T,
        cross_min: T,
        cross_max: T,
        height: usize,
    ) -> (usize, usize) {

        let mut bot_min = T::infinity();
        let mut top_max = T::neg_infinity();

        for trapz in self.advance_to(right) {
            top_max = top_max.max(trapz.left.top).max(trapz.right.top);
            bot_min = bot_min.min(trapz.left.bot).min(trapz.right.bot);
        }

        if top_max <= bot_min || top_max <= cross_min || bot_min >= cross_max {
            return (height, 0);
        }
        // *. top_max >= top_min; bot_max >= bot_min
        //    otherwise, we didn't see any item
        // intersection: [bot_min, top_max]
        bot_min = bot_min.max(cross_min);
        top_max = top_max.min(cross_max);

        // *. set up transform to pixels
        let height_t = T::from(height).unwrap();
        let slice_height_inv =  height_t / (cross_max - cross_min);
        let cross_idx = |y: T| {
            let j: T = (y - cross_min) * slice_height_inv;
            let j = j.max(T::zero()).min(height_t);
            let j = j.floor().to_usize().unwrap();
            j.min(height - 1)
        };


        let j_min = cross_idx(bot_min);
        let j_max = cross_idx(top_max);

        (j_min, j_max)
    }
    pub fn cross_bounds(
        &mut self,
        right: T,
        cross_min: T,
        cross_max: T,
        height: usize,
    ) -> Option<(usize, usize, T, T)> {

        let mut bot_min = T::infinity();
        let mut top_max = T::neg_infinity();

        let mut top_min = T::infinity();
        let mut bot_max = T::neg_infinity();

        for trapz in self.advance_to(right) {
            top_max = top_max.max(trapz.left.top).max(trapz.right.top);
            top_min = top_min.min(trapz.left.top).min(trapz.right.top);
            bot_max = bot_max.max(trapz.left.bot).max(trapz.right.bot);
            bot_min = bot_min.min(trapz.left.bot).min(trapz.right.bot);
        }

        // *. top_max >= top_min; bot_max >= bot_min
        //    otherwise, we didn't see any item
        if top_max <= bot_min || top_max <= cross_min || bot_min >= cross_max {
            return None;
        }

        // intersection: [bot_min, top_max]
        bot_min = bot_min.max(cross_min);
        top_max = top_max.min(cross_max);

        // *. set up transform to pixels
        let height_t = T::from(height).unwrap();
        let slice_height_inv =  height_t / (cross_max - cross_min);
        let cross_idx = |y: T| {
            let j: T = (y - cross_min) * slice_height_inv;
            let j = j.max(T::zero()).min(height_t);
            let j = j.floor().to_usize().unwrap();
            j.min(height - 1)
        };


        let j_min = cross_idx(bot_min);
        let j_max = cross_idx(top_max);

        Some((j_min, j_max, bot_max, top_min))
    }

    pub fn cross_area(
        &mut self,
        right: T,
        cross_min: T,
        cross_max: T,
        array: &mut [T],
    ) {
        let height = array.len();

        // *. set up transform to pixels
        let height_t = T::from(height).unwrap();
        let slice_height = (cross_max - cross_min) / height_t;
        let cross_idx = |y: T| {
            let j_t: T = (y - cross_min) / slice_height;
            let j_t = j_t.max(T::zero()).min(height_t);
            let j = j_t.floor().to_usize().unwrap();
            j.min(height - 1)
        };

        let cross_pt = |j: usize| {
            T::from(j).unwrap() * slice_height + cross_min
        };

        for trapz in self.advance_to(right) {
            let left = trapz.left;
            let right = trapz.right;

            let mut top_coords: Vec<Coordinate<_>> = vec![];
            let mut bot_coords: Vec<Coordinate<_>> = vec![];
            if left.bot <= right.bot {
                // left-bot is the left-most coord after flip.
                top_coords.push((left.bot, left.x).into());
                bot_coords.push((left.bot, left.x).into());

                // since right > left, it flips to the top.
                top_coords.push((right.bot, right.x).into());
                if right.top > right.bot {
                    top_coords.push((right.top, right.x).into());
                }
                if left.top > left.bot {
                    bot_coords.push((left.top, left.x).into());
                }
            } else {
                // right-bot is the left-most coord after flip.
                top_coords.push((right.bot, right.x).into());
                bot_coords.push((right.bot, right.x).into());

                if right.top > right.bot {
                    top_coords.push((right.top, right.x).into());
                }
                bot_coords.push((left.bot, left.x).into());
                if left.top > left.bot {
                    bot_coords.push((left.top, left.x).into());
                }
            }

            // lt is in bot_coords, rt in top_c
            if left.top <= right.top {
                bot_coords.push((right.top, right.x).into());
            } else {
                top_coords.push((left.top, left.x).into());
            }
            let tpoly = MonoPoly::new(LineString(top_coords), LineString(bot_coords));
            let mut scanner = tpoly.scan_lines(cross_min);

            let bot_min = left.bot.min(right.bot);
            let top_max = left.top.max(right.top);

            if bot_min >= cross_max || top_max <= cross_min {
                continue;
            }

            let j_min = cross_idx(bot_min);
            let j_max = cross_idx(top_max);
            for j in j_min..=j_max {
                let jt = cross_pt(j + 1);
                for tz in scanner.advance_to(jt) {
                    array[j] = array[j] + tz.area();
                }
            }
        }
    }

}
