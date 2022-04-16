use std::iter::FromIterator;

use geo::{winding_order::WindingOrder, GeoNum, LineString};
use log::debug;
use smallvec::SmallVec;

use super::{winding_inverse, Chain, Link, Sweep};

const CHAIN_STACK_SIZE: usize = 16;
type Chains<T> = SmallVec<[Chain<T>; CHAIN_STACK_SIZE]>;

pub fn monotone_chains<T: GeoNum + Unpin>(
    ring: LineString<T>,
) -> Vec<(LineString<T>, LineString<T>)> {
    let mut chains = Chains::<T>::new();

    let finalize_pair = |p1: &Chain<T>, p2: &Chain<T>| {
        debug_assert_ne!(p1.interior(), p2.interior());
        let (p1, p2) = if p1.interior() == &WindingOrder::Clockwise {
            (p1, p2)
        } else {
            (p2, p1)
        };
        debug!(
            "chain: finalize: {top},{bot}",
            top = p2.other(),
            bot = p1.other()
        );
    };

    let sweep = Box::pin(Sweep::from_closed_ring(ring));
    for event in sweep {
        let mut curr_idx = None;
        let pt = event.pt;

        debug!("event {ty:?} @ {pt:?}", ty = event.ty);
        for link in event.links {
            debug!("link: {link:?}");
            match link {
                Link::Start { root, top, bot } => {
                    debug_assert_eq!(root, event.pt);
                    // Create two chains with orientation, and "looking at top/bot"
                    let tdx = chains.len();
                    let bdx = tdx + 1;
                    let top_chain = Chain::new(
                        WindingOrder::Clockwise,
                        bdx,
                        FromIterator::from_iter([root, top]),
                    );
                    let bot_chain = Chain::new(
                        WindingOrder::CounterClockwise,
                        tdx,
                        FromIterator::from_iter([root, bot]),
                    );
                    chains.extend([top_chain, bot_chain]);
                    debug!("chain: create: {tdx} ({top:?}) and {bdx} ({bot:?})");
                }
                Link::Continue {
                    prev,
                    curr,
                    next,
                    interior: _,
                } => {
                    debug_assert_eq!(curr, event.pt);
                    // Find unique chain looking at curr, and move it.
                    let (idx, chain) = chains
                        .iter_mut()
                        .enumerate()
                        .find(|&(_i, ref ch): &(usize, &mut Chain<T>)| {
                            !ch.done() && ch.next() == curr && ch.curr() == prev
                        })
                        .expect("chain for continue");
                    curr_idx = Some(idx);
                    debug!("chain: advance {idx} to {next:?}");
                    chain.advance(next);
                }
                Link::Merge { prev, next } => {
                    debug_assert_eq!(next, event.pt);
                    let num = chains
                        .iter_mut()
                        .enumerate()
                        .filter(|&(_i, ref ch): &(usize, &mut Chain<T>)| {
                            !ch.done() && ch.next() == prev
                        })
                        .map(|(i, ch)| {
                            ch.advance(next);
                            debug!("chain: advance {i} to {next:?}");
                        })
                        .count();

                    debug_assert_eq!(num, 2);
                }
                Link::Split {
                    prev,
                    next,
                    top,
                    bot,
                } => {
                    debug_assert_eq!(next, event.pt);
                    // Find unique chain at prev, and move it.
                    let (idx, chain) = chains
                        .iter_mut()
                        .enumerate()
                        .find(|&(_i, ref ch): &(usize, &mut Chain<T>)| {
                            !ch.done() && ch.curr() == prev
                        })
                        .expect("chain for split");

                    let old_next = chain.pop();
                    debug!(
                        "chain: advance {idx} to {next:?} (int = {intr:?})",
                        intr = chain.interior()
                    );
                    chain.advance(next);

                    let other = if chain.interior() == &WindingOrder::Clockwise {
                        debug!("chain: advance {idx} to {bot:?}");
                        chain.advance(bot);
                        top
                    } else {
                        debug!("chain: advance {idx} to {top:?}");
                        chain.advance(top);
                        bot
                    };
                    let chain_interior = chain.interior().clone();

                    let tdx = chains.len();
                    let bdx = tdx + 1;
                    let chain1 = Chain::new(
                        chain_interior.clone(),
                        bdx,
                        FromIterator::from_iter([prev, old_next]),
                    );
                    let chain2 = Chain::new(
                        winding_inverse(chain_interior),
                        tdx,
                        FromIterator::from_iter([prev, next, other]),
                    );
                    chains.extend([chain1, chain2]);
                    debug!("chain: create[split]: {tdx} ({old_next:?}) and {bdx} ({other:?})");
                }
                Link::End {
                    top: _,
                    bot: _,
                    sink,
                } => {
                    debug_assert_eq!(sink, event.pt);
                }
            }
        }
        let looks = SmallVec::<[usize; 8]>::from_iter(chains.iter().enumerate().filter_map(
            |(idx, chain)| {
                if chain.done() || chain.next() != pt {
                    None
                } else {
                    Some(idx)
                }
            },
        ));
        debug_assert_eq!(looks.len() % 2, 0);
        debug!("looks: {len}", len = looks.len());

        // Reduce current point
        match curr_idx {
            Some(curr_idx) if looks.len() > 0 => {
                debug_assert_eq!(looks.len(), 2);
                let (pair, other) = if chains[looks[0]].other() == curr_idx {
                    (looks[0], looks[1])
                } else if chains[looks[1]].other() == curr_idx {
                    (looks[1], looks[0])
                } else {
                    panic!("no merge idx matches curr.other!");
                };
                // Close {pair, curr}, move other into curr
                let new_next = chains[curr_idx].pop();
                debug_assert_eq!(chains[curr_idx].next(), chains[pair].next());
                debug!("reduce: curr = {curr_idx} with {pair} and {other}");

                chains[curr_idx].finish();
                chains[pair].finish();
                finalize_pair(&chains[curr_idx], &chains[pair]);

                chains[other].advance(new_next);
                debug!("chain: advance: {other} to {new_next:?}");
            }
            None if looks.len() > 0 => {
                debug_assert_eq!(looks.len() % 2, 0);
                for idx in looks {
                    let ch1 = &chains[idx];
                    let jdx = ch1.other();
                    let ch2 = &chains[jdx];
                    debug_assert_eq!(ch1.done(), ch2.done());
                    if ch1.done() {
                        continue;
                    }

                    if ch1.next() == ch2.next() {
                        chains[idx].finish();
                        chains[jdx].finish();
                        finalize_pair(&chains[idx], &chains[jdx]);
                    }
                }
            }
            _ => {}
        }
    }

    let mut pairs = vec![];
    let mut lines: Vec<_> = chains
        .into_iter()
        .enumerate()
        .map(|(i, chain)| {
            debug_assert!(chain.done());
            if chain.interior() == &WindingOrder::Clockwise {
                pairs.push((i, chain.other()))
            }
            chain
        })
        .map(LineString::from)
        .collect();
    debug_assert_eq!(pairs.len() * 2, lines.len());
    pairs
        .into_iter()
        .map(|(topdx, botdx)| {
            let top_ls = std::mem::replace(&mut lines[topdx], LineString(vec![]));
            let bot_ls = std::mem::replace(&mut lines[botdx], LineString(vec![]));
            (top_ls, bot_ls)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{crossings::tests::init_log, SweepPoint};
    use approx::assert_relative_eq;
    use geo::{map_coords::MapCoords, prelude::Area, Coordinate, LineString, Polygon};

    fn verify_monotone(ring: LineString<f64>) {
        let true_area = Polygon::new(ring.clone(), vec![]).unsigned_area();
        let total_area: f64 = monotone_chains(ring)
            .into_iter()
            .map(|(top, down)| {
                eprintln!("Pair:");
                eprintln!("\t{top:?}");
                eprintln!("\t{down:?}");

                let mut top = top.0;
                let mut down = down.0;

                for win in top.windows(2).chain(down.windows(2)) {
                    assert!(SweepPoint::from(win[0]) < win[1].into());
                }

                down.reverse();
                assert_eq!(down.first(), top.last());
                top.extend(down.drain(1..));

                let geom = LineString(top);
                assert!(geom.is_closed());

                Polygon::new(geom, vec![]).unsigned_area()
            })
            .sum();
        eprintln!("{total_area} vs {true_area}");
        assert_relative_eq!(total_area, true_area, epsilon = 1.0e-6);
    }

    #[test]
    fn check_chains_iter() {
        init_log();

        let ring = LineString(vec![
            Coordinate::<f64>::from([0., 0.]),
            [-1., 1.].into(),
            [0., 2.].into(),
            [2., 0.].into(),
            [0., -2.].into(),
            [-1., -1.].into(),
        ]);
        let mirror: LineString<_> = ring.map_coords(|&(x, y)| (-x, y));

        verify_monotone(ring);
        eprintln!("==================================== mirror =================================");
        verify_monotone(mirror);
    }
}
