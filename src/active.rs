use slab::Slab;
use std::{cmp::Ordering, collections::BTreeSet, fmt::Debug, ops::Bound};

use crate::SplaySet;

/// Internal representation used in ordered sets.
pub struct Active<T> {
    key: usize,
    storage: *const Slab<T>,
}

impl<T: Debug> Debug for Active<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveSegment")
            .field("key", &self.key)
            .field("segment", &self.get())
            .finish()
    }
}

impl<T> Active<T> {
    /// Create a new active segment pointing to the given storage.
    ///
    /// # Safety
    ///
    /// This function is unsafe. Caller must ensure that:
    ///
    /// 1. the `storage` reference is valid _through-out_ the
    /// lifetime of the created object
    ///
    /// 2. all co-existing active-segments are
    /// linearly-orderable as per `T: PartialOrd`, and the
    /// ordering does not change over its lifetime.
    /// Violating this will not lead to a memory-UB, but may
    /// cause panics or incorrect output.
    unsafe fn new(key: usize, storage: &Slab<T>) -> Self {
        Active {
            key,
            storage: storage as *const _,
        }
    }
    fn get(&self) -> &T {
        // Safety: reference is guaranteed to be valid by the `new`
        // method.
        let slab = unsafe { &*self.storage as &Slab<_> };
        unsafe { slab.get_unchecked(self.key) }
    }
}

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<T> PartialEq for Active<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Assert total equality.
impl<T: PartialOrd> Eq for Active<T> {}

/// Partial ordering defined as per algorithm.
///
/// This is requires the same pre-conditions as for [`LineOrPoint`].
impl<T: PartialOrd> PartialOrd for Active<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get()
            .partial_cmp(
                other
                    .get()
            )
    }
}

/// Assert total ordering same as `PartialOrd` impl.
impl<T: PartialOrd + Debug> Ord for Active<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .unwrap_or_else(|| {
                panic!("couldn't compare active segments: {self:?} <=> {other:?}", self = self);
            })
    }
}

/// Helper trait to insert, remove and get adjacent segments from ordered set.
pub trait Access: Default {
    type SegmentType;
    fn prev_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize>;
    fn next_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize>;
    unsafe fn add_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
    fn remove_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
}

impl<T: PartialOrd + Debug> Access for BTreeSet<Active<T>> {
    type SegmentType = T;

    #[inline]
    fn prev_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding an immut. reference to the storage.
        let aseg = unsafe { Active::new(key, storage) };
        self.range((Bound::Unbounded, Bound::Excluded(aseg)))
            .next_back()
            .map(|s| s.key)
    }

    #[inline]
    fn next_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding a immut. reference to the storage.
        let aseg = unsafe { Active::new(key, storage) };
        self.range((Bound::Excluded(aseg), Bound::Unbounded))
            .next()
            .map(|s| s.key)
    }

    #[inline]
    unsafe fn add_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        assert!(self.insert(Active::new(key, storage)));
    }

    #[inline]
    fn remove_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        // Safety: temporary active segment is valid as we're holding
        // a immut. reference to `storage`.
        assert!(self.remove(&unsafe { Active::new(key, storage) }));
    }
}

pub struct SplayWrap<T: Ord>(SplaySet<T, Box<dyn Fn(&T, &T) -> Ordering>>);

impl<T: Ord> Default for SplayWrap<T> {
    fn default() -> Self {
        Self(SplaySet::new(Box::new(|a: &T, b: &T| a.cmp(b))))
    }
}

impl<T: PartialOrd + Debug> Access for SplayWrap<Active<T>> {
    type SegmentType = T;

    fn prev_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding an immut. reference to the storage.
        let aseg = unsafe { Active::new(key, storage) };
        self.0.prev(&aseg)
            .map(|s| s.key)
    }

    fn next_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding an immut. reference to the storage.
        let aseg = unsafe { Active::new(key, storage) };
        self.0.next(&aseg)
            .map(|s| s.key)
    }

    unsafe fn add_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        assert!(self.0.insert(Active::new(key, storage)));
    }

    fn remove_key(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        // Safety: temporary active segment is valid as we're holding
        // a immut. reference to `storage`.
        assert!(self.0.remove(&unsafe { Active::new(key, storage) }));
    }
}
