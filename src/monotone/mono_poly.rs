use geo::{
    line_interpolate_point::LineInterpolatePoint, Coordinate, GeoFloat, GeoNum, Line, LineString,
    Polygon,
};
use std::{cmp::Ordering, iter::from_fn};

use crate::SweepPoint;

use super::{Trapz, Trip};

#[derive(Clone)]
pub struct MonoPoly<T: GeoNum> {
    top: LineString<T>,
    bot: LineString<T>,
}

impl<T: GeoNum> std::fmt::Debug for MonoPoly<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let top: Vec<SweepPoint<T>> = self.top.0.iter().map(|c| (*c).into()).collect();
        let bot: Vec<SweepPoint<T>> = self.bot.0.iter().map(|c| (*c).into()).collect();
        f.debug_struct("MonoPoly")
            .field("top", &top)
            .field("bot", &bot)
            .finish()
    }
}

impl<T: GeoNum> MonoPoly<T> {
    pub fn new(top: LineString<T>, bot: LineString<T>) -> Self {
        assert_eq!(top.0.first(), bot.0.first());
        assert_eq!(top.0.last(), bot.0.last());
        assert_ne!(top.0.first(), top.0.last());

        for win in top.0.windows(2).chain(bot.0.windows(2)) {
            if SweepPoint::from(win[0]) >= SweepPoint::from(win[1]) {
                eprintln!("ERR: {:?} >= {:?}", win[0], win[1]);
            }
            assert!(SweepPoint::from(win[0]) < SweepPoint::from(win[1]));
        }
        Self { top, bot }
    }

    /// Get a reference to the mono poly's top.
    #[must_use]
    pub fn top(&self) -> &LineString<T> {
        &self.top
    }

    /// Get a reference to the mono poly's bot.
    #[must_use]
    pub fn bot(&self) -> &LineString<T> {
        &self.bot
    }

    pub fn into_ls_pair(self) -> (LineString<T>, LineString<T>) {
        (self.top, self.bot)
    }

    pub fn into_polygon(self) -> Polygon<T> {
        let mut down = self.bot.0;
        let mut top = self.top.0;

        down.reverse();
        assert_eq!(down.first(), top.last());
        top.extend(down.drain(1..));

        let geom = LineString(top);
        debug_assert!(geom.is_closed());

        Polygon::new(geom, vec![])
    }
}

impl<T: GeoFloat> MonoPoly<T> {
    #[inline]
    pub fn scan_lines(&self, left: T) -> Scanner<'_, T> {
        let mut scanner = Scanner::new(self);
        scanner.advance_to(left).count();
        scanner
    }
}

pub struct Scanner<'a, T: GeoFloat> {
    poly: &'a MonoPoly<T>,
    curr: Trip<T>,
    top_idx: usize,
    bot_idx: usize,
}

impl<'a, T: GeoFloat> Scanner<'a, T> {
    pub fn new(poly: &'a MonoPoly<T>) -> Self {
        let top = poly.top().0[0];
        let bot = poly.bot().0[0];
        debug_assert_eq!(top, bot);

        Self {
            poly,
            curr: Trip {
                x: top.x,
                top: top.y,
                bot: bot.y,
            },
            top_idx: 0,
            bot_idx: 0,
        }
    }

    pub fn cross_bounds(
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

        // *. top_max >= top_min; bot_max >= bot_min
        //    otherwise, we didn't see any item
        if top_max < bot_min {
            return (height, 0);
        }
        // intersection: [bot_min, top_max]
        bot_min = bot_min.max(cross_min);
        top_max = top_max.min(cross_max);
        if top_max < bot_min {
            return (height, 0);
        }

        // *. set up transform to pixels
        let height_t = T::from(height).unwrap();
        let slice_height = (cross_max - cross_min) / height_t;
        let cross_idx = |y: T| {
            let j: T = (y - cross_min) / slice_height;
            let j = j.max(T::zero()).min(height_t);
            let j = j.floor().to_usize().unwrap();
            j.min(height - 1)
        };


        let j_min = cross_idx(bot_min);
        let j_max = cross_idx(top_max);

        (j_min, j_max)
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

    #[inline]
    pub fn advance_to<'b>(&'b mut self, until: T) -> impl Iterator<Item = Trapz<T>> + 'b
    where
        T: 'a,
    {
        // The invariance stuff is too deep.
        // Some day, we should understand:
        // https://users.rust-lang.org/t/why-mut-t-is-not-covariant-with-t/54944
        let this: &mut Scanner<'b, T> = unsafe { std::mem::transmute(self) };
        from_fn(move || {
            if this.curr.x >= until {
                return None;
            }
            this.advance_next(until)
        })
        .flatten()
    }

    pub fn advance_upto<F: FnMut(Trapz<T>)>(&mut self, until: T, mut proc: F) {
        while self.curr.x < until {
            if let Some(result) = self.advance_next(until) {
                if let Some(result) = result {
                    proc(result);
                }
            } else {
                return;
            }
        }
    }

    pub fn advance_next(&mut self, until: T) -> Option<Option<Trapz<T>>> {
        // Precondition: self.curr.x < until
        debug_assert!(self.curr.x < until);
        let mut result = None;

        let (ord, next) = match self.peek_next(until) {
            Some(e) => e,
            None => return None,
        };

        // peek_next guarantees: next.x <= x
        // yield a non-degenerate trapz: (curr, next)
        if self.curr.x < next.x {
            result = Some((self.curr, next).into());
        }

        // Increment curr
        self.curr = next;

        if let Some(ord) = ord {
            match ord {
                Ordering::Less => {
                    self.top_idx += 1;
                }
                Ordering::Equal => {
                    self.top_idx += 1;
                    self.bot_idx += 1;
                }
                Ordering::Greater => {
                    self.bot_idx += 1;
                }
            }
        }

        Some(result)
    }

    fn peek_next(&self, until: T) -> Option<(Option<Ordering>, Trip<T>)> {
        let top = &self.poly.top().0;
        let bot = &self.poly.bot().0;
        let top_val = if self.top_idx + 1 >= top.len() {
            // already at last in the chain
            None
        } else {
            Some(top[self.top_idx + 1])
        };
        let bot_val = if self.bot_idx + 1 >= bot.len() {
            // already at last in the chain
            None
        } else {
            Some(bot[self.bot_idx + 1])
        };

        let interpolate = |p1: Coordinate<T>, p2: Coordinate<T>, x| -> T {
            debug_assert!(p1.x <= x && x <= p2.x);
            if p2.x == p1.x {
                return p2.y;
            }
            let frac = (x - p1.x) / (p2.x - p1.x);
            Line::new(p1, p2)
                .line_interpolate_point(frac)
                .expect("interpolate mono-poly segment")
                .0
                .y
        };
        let (new_top, new_bot, ord, new_x) = match (top_val, bot_val) {
            (None, None) => return None,
            (Some(new_top), Some(new_bot)) => {
                let ord = new_top
                    .x
                    .partial_cmp(&new_bot.x)
                    .expect("coords. must be orderable");
                let new_x = match ord {
                    Ordering::Less => new_top.x,
                    _ => new_bot.x,
                };
                (new_top, new_bot, ord, new_x)
            }
            (None, Some(new_bot)) => {
                debug_assert_eq!(new_bot.x, self.curr.x);
                (top[self.top_idx], new_bot, Ordering::Greater, self.curr.x)
            }
            (Some(new_top), None) => {
                debug_assert_eq!(new_top.x, self.curr.x);
                (new_top, bot[self.bot_idx], Ordering::Less, self.curr.x)
            }
        };
        if until < new_x {
            Some((
                None,
                Trip {
                    x: until,
                    top: interpolate(top[self.top_idx], new_top, until),
                    bot: interpolate(bot[self.bot_idx], new_bot, until),
                },
            ))
        } else {
            match ord {
                Ordering::Less => Some((
                    Some(ord),
                    Trip {
                        x: new_top.x,
                        top: new_top.y,
                        bot: interpolate(bot[self.bot_idx], new_bot, new_top.x),
                    },
                )),
                Ordering::Equal => Some((
                    Some(ord),
                    Trip {
                        x: new_top.x,
                        top: new_top.y,
                        bot: new_bot.y,
                    },
                )),
                Ordering::Greater => Some((
                    Some(ord),
                    Trip {
                        x: new_bot.x,
                        top: interpolate(top[self.top_idx], new_top, new_bot.x),
                        bot: new_bot.y,
                    },
                )),
            }
        }
    }
}
