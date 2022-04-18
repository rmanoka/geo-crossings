use std::{cmp::Ordering, iter};

use geo::{
    line_interpolate_point::LineInterpolatePoint, Coordinate, GeoFloat, GeoNum, Line, LineString,
    Polygon,
};

use crate::SweepPoint;

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
    pub fn scan_lines(&self, left: T) -> Scanner<'_, T> {
        let mut scanner = Scanner::new(self);
        scanner.advance_to(left).count();
        scanner
    }
}

impl<T: GeoFloat> MonoPoly<T> {}

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

    pub fn advance_to<'b>(&'b mut self, until: T) -> impl Iterator<Item = Trapz<T>> + 'b
    where
        T: 'a,
    {
        // The invariance stuff is too deep.
        // Some day, we should understand:
        // https://users.rust-lang.org/t/why-mut-t-is-not-covariant-with-t/54944
        let this: &mut Scanner<'b, T> = unsafe { std::mem::transmute(self) };
        iter::from_fn(move || {
            if this.curr.x >= until { return None; }
            this.advance_next(until)
        }).flatten()
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Trip<T: GeoNum> {
    pub x: T,
    pub top: T,
    pub bot: T,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Trapz<T: GeoNum> {
    left: Trip<T>,
    right: Trip<T>,
}

impl<T: GeoNum> From<(Trip<T>, Trip<T>)> for Trapz<T> {
    fn from(tup: (Trip<T>, Trip<T>)) -> Self {
        Trapz {
            left: tup.0,
            right: tup.1,
        }
    }
}

impl<T: GeoFloat> Trapz<T> {
    #[inline]
    pub fn area(&self) -> T {
        let t1 = &self.left;
        let t2 = &self.right;

        let a = t1.top - t1.bot;
        let b = t2.top - t2.bot;
        debug_assert!(a >= T::zero() && b >= T::zero());
        let h = t2.x - t1.x;
        let two = T::one() + T::one();
        (a + b) / two * h
    }
}

#[cfg(test)]
mod tests {
    use approx::assert_relative_eq;
    use geo::prelude::{Area, BoundingRect};
    use log::info;

    use crate::crossings::tests::init_log;
    use crate::monotone_chains;

    use super::super::tests::*;
    use super::*;

    #[test]
    fn check_ops_scanner() {
        init_log();
        verify_scanner_area(&poly_simple_merge());
        verify_scanner_area(&poly_simple_split());
    }

    fn verify_scanner_area(poly: &Polygon<f64>) {
        let bbox = poly
            .bounding_rect()
            .expect("check_ops_scanner: bounding_rect");

        let left = bbox.min().x;
        let right = bbox.max().x;

        const NUM_SLICES: usize = 1000000;
        let slice_width = (right - left) / NUM_SLICES as f64;

        let true_area = poly.unsigned_area();
        let total_area: f64 = monotone_chains(poly)
            .into_iter()
            .map(|mono| -> f64 {
                let mut left = left;
                info!("processing mono-polygon: {mono:?}");
                let mut scanner = mono.scan_lines(left);

                (0..NUM_SLICES)
                    .map(|_| {
                        let right = (left + slice_width).min(right);

                        let mut area = 0.;
                        for trapz in scanner.advance_to(right) {
                            area += trapz.area();
                        }
                        left = right;
                        area
                    })
                    .sum()
            })
            .sum();
        info!("{total_area} vs {true_area}");
        assert_relative_eq!(total_area, true_area, epsilon = 1.0e-6);
    }
}
