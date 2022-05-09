use slab::Slab;
use std::{cmp::Ordering, collections::BTreeSet, fmt::Debug, ops::Bound};

/// Internal representation used in ordered sets.
pub(crate) struct ActiveSegment<T> {
    key: usize,
    storage: *const Slab<T>,
}

impl<T: Debug> Debug for ActiveSegment<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveSegment")
            .field("key", &self.key)
            .field("segment", &self.get())
            .finish()
    }
}

impl<T> ActiveSegment<T> {
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
        ActiveSegment {
            key,
            storage: storage as *const _,
        }
    }
    fn get(&self) -> Option<&T> {
        // Safety: reference is guaranteed to be valid by the `new`
        // method.
        let slab = unsafe { &*self.storage as &Slab<_> };
        slab.get(self.key)
    }
}

/// Partial equality based on key.
///
/// This is consistent with the `PartialOrd` impl.
impl<T> PartialEq for ActiveSegment<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

/// Assert total equality.
impl<T: PartialOrd> Eq for ActiveSegment<T> {}

/// Partial ordering defined as per algorithm.
///
/// This is requires the same pre-conditions as for [`LineOrPoint`].
impl<T: PartialOrd> PartialOrd for ActiveSegment<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get()
            .expect("ActiveSegment::partial_cmp: could not find key in storage")
            .partial_cmp(
                other
                    .get()
                    .expect("ActiveSegment::partial_cmp: could not find key in storage"),
            )
    }
}

/// Assert total ordering same as `PartialOrd` impl.
impl<T: PartialOrd> Ord for ActiveSegment<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .expect("unable to compare active segments!")
    }
}

/// Helper trait to insert, remove and get adjacent segments from ordered set.
pub(crate) trait SegmentAccess {
    type SegmentType;
    fn prev_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize>;
    fn next_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize>;
    unsafe fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>);
}

impl<T: PartialOrd> SegmentAccess for BTreeSet<ActiveSegment<T>> {
    type SegmentType = T;

    #[inline]
    fn prev_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding an immut. reference to the storage.
        let aseg = unsafe { ActiveSegment::new(key, storage) };
        self.range((Bound::Unbounded, Bound::Excluded(aseg)))
            .next_back()
            .map(|s| s.key)
    }

    #[inline]
    fn next_key(&self, key: usize, storage: &Slab<Self::SegmentType>) -> Option<usize> {
        // Safety: aseg is only valid till end of function, and we
        // are holding a immut. reference to the storage.
        let aseg = unsafe { ActiveSegment::new(key, storage) };
        self.range((Bound::Excluded(aseg), Bound::Unbounded))
            .next()
            .map(|s| s.key)
    }

    #[inline]
    unsafe fn add_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        assert!(self.insert(ActiveSegment::new(key, storage)));
    }

    #[inline]
    fn remove_segment(&mut self, key: usize, storage: &Slab<Self::SegmentType>) {
        debug_assert!(storage.contains(key));
        // Safety: temporary active segment is valid as we're holding
        // a immut. reference to `storage`.
        assert!(self.remove(&unsafe { ActiveSegment::new(key, storage) }));
    }
}
