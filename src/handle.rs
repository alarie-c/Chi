use std::{fmt::Debug, marker::PhantomData, ops::Index};

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Null Type Underlying
// ------------------------------------------------------------------------------------------------------------------ //

/// Implement this trait for any underyling types of a handle to denote that this handle can point to a "null"
/// object and what the index of said "null" object is.
///
/// The handle has nothing to do with the underlying type being null. Nullness is defined by the underlying type
/// implementing this trait. If `handle.index()` is equivalent to `null_idx()` here, then the handle is defined
/// to be null.
pub trait NullHandleUnderlying {
    /// Handle index to be defined as "null" for this type.
    fn null_idx() -> usize;
}

impl<T: NullHandleUnderlying> Handle<T> {
    /// Returns whether or not the handle contains the "null" index as defined by the underlying type `T`.
    pub fn is_null(&self) -> bool {
        self.idx == T::null_idx()
    }

    /// Returns a handle with the "null" index.
    pub fn null() -> Self {
        Self {
            idx: T::null_idx(),
            _pd: PhantomData,
        }
    }
}

// ------------------------------------------------------------------------------------------------------------------ //
// MARK: Handle
// ------------------------------------------------------------------------------------------------------------------ //

/// Used to point to some element of `T` in an arena while maintaining type safety.
/// Preferred to lugging around `usize` or semantically unsafe `usize` aliases.
#[derive(Hash)]
pub struct Handle<T> {
    /// The index of the item in the arena.
    idx: usize,
    _pd: PhantomData<fn() -> T>,
}

impl<T> Handle<T> {
    /// Creates a new handle from the specified index.
    pub const fn new(idx: usize) -> Self {
        Self {
            idx,
            _pd: PhantomData,
        }
    }

    /// Returns the index of the handle.
    pub fn index(&self) -> usize {
        self.idx
    }
}

//
// Manual Trait Implementations
//

// Simplifies the debugged layout of handle to fit on one line and remove the phantom data from being printed.
impl<T> Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Handle({})", self.idx)
    }
}

// Simplifies the equality operation to only match the index instead of the index and phantom data.
// Comparing handles of the same type T is already guaranteed through generics.
impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

// Allows handles to index directly into a vector.
impl<T> Index<Handle<T>> for Vec<T> {
    type Output = T;
    fn index(&self, h: Handle<T>) -> &Self::Output {
        &self[h.index()]
    }
}

// These have to be manually derived, otherwise the compiler will expect the underlying type T to
// implement these as well (which most of the time they cant).

impl<T> Eq for Handle<T> {}
impl<T> Copy for Handle<T> {}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}
