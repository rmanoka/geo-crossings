use geo::{
    Coordinate, GeoFloat, GeoNum, LineString,
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
            while this.curr.x < until {
                match this.advance_next(until) {
                    Some(Some(t)) => return Some(t),
                    None => return None,
                    _ => {},
                }
            }
            None
        })
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
            let y = frac * (p2.y - p1.y) + p1.y;

            let (min_y, max_y) = if p1.y > p2.y {
                (p2.y, p1.y)
            } else {
                (p1.y, p2.y)
            };

            y.max(min_y).min(max_y)
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

    /// Get the scanner's curr.
    #[must_use]
    pub fn curr(&self) -> Trip<T> {
        self.curr
    }
}
