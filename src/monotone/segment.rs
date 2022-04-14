use geo::{winding_order::WindingOrder, Coordinate, GeoNum, Line};
use log::debug;
use std::cmp::Ordering;

use crate::{
    events::{Event, EventType},
    line_or_point::LineOrPoint,
    monotone::winding_inverse,
    SweepPoint,
};

#[derive(Debug, Clone)]
pub struct Helper<T: GeoNum> {
    point: SweepPoint<T>,
    is_merge: bool,
}

impl<T: GeoNum> Helper<T> {
    pub fn set_merge(&mut self) {
        self.is_merge = true;
    }

    pub fn reset_merge(&mut self) {
        self.is_merge = false;
    }

    /// Get the helper's is merge.
    #[must_use]
    pub fn is_merge(&self) -> bool {
        self.is_merge
    }

    /// Get the helper's point.
    #[must_use]
    pub fn point(&self) -> SweepPoint<T> {
        self.point
    }

    /// Get a mutable reference to the helper's point.
    #[must_use]
    pub fn point_mut(&mut self) -> &mut SweepPoint<T> {
        &mut self.point
    }
}

#[derive(Debug, Clone)]
pub struct Segment<T: GeoNum> {
    key: usize,
    pub geom: LineOrPoint<T>,
    helper: Option<Helper<T>>,
}

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<T: GeoNum> PartialEq for Segment<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Partial ordering defined as per algorithm.
///
/// This requires the same pre-conditions as for [`LineOrPoint`].
impl<T: GeoNum> PartialOrd for Segment<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // let other_geom = LineOrPoint::Line(other.start, other.end);
        self.geom
            .partial_cmp(&other.geom)
            .map(|o| o.then_with(|| self.key.cmp(&other.key)))
    }
}

impl<T: GeoNum> Segment<T> {
    pub fn new_point(key: usize, geom: Coordinate<T>) -> Self {
        Segment {
            key,
            geom: LineOrPoint::Point(geom.into()),
            helper: None,
        }
    }

    pub fn new(key: usize, geom: Line<T>, mut winding: WindingOrder) -> Self {
        let mut start = geom.start.into();
        let mut end = geom.end.into();
        debug_assert_ne!(start, end);
        if start > end {
            std::mem::swap(&mut start, &mut end);
            winding = winding_inverse(winding);
        }
        let helper = if winding == WindingOrder::Clockwise {
            Some(Helper {
                point: start,
                is_merge: false,
            })
        } else {
            None
        };
        let geom = LineOrPoint::Line(start, end);
        let this = Segment { key, geom, helper };
        debug!("new segment: {this:?}");
        this
    }

    pub(crate) fn events(&self) -> [Event<T>; 2] {
        assert!(self.geom.is_line());
        let geom = self.geom.line();
        [
            Event {
                point: geom.start.into(),
                ty: EventType::LineLeft,
                segment_key: self.key,
            },
            Event {
                point: geom.end.into(),
                ty: EventType::LineRight,
                segment_key: self.key,
            },
        ]
    }

    pub fn interior_winding(&self) -> WindingOrder {
        if self.helper.is_some() {
            WindingOrder::Clockwise
        } else {
            WindingOrder::CounterClockwise
        }
    }

    /// Get a reference to the segment's helper.
    #[must_use]
    pub fn helper(&self) -> Option<&Helper<T>> {
        self.helper.as_ref()
    }

    /// Segment's inner setter.
    pub fn helper_mut(&mut self) -> Option<&mut Helper<T>> {
        self.helper.as_mut()
    }
}
