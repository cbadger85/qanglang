use core::mem;

/// A minimal generational arena implementation
#[derive(Clone, Debug)]
pub struct Arena<T> {
    items: Vec<Entry<T>>,
    generation: u32,
    free_list_head: Option<u32>,
    len: usize,
}

#[derive(Clone, Debug)]
enum Entry<T> {
    Free { next_free: Option<u32> },
    Occupied { generation: u32, value: T },
}

/// An index (and generation) into an `Arena`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index {
    index: u32,
    generation: u32,
}

impl Default for Index {
    fn default() -> Self {
        Index {
            index: u32::MAX,
            generation: u32::MAX,
        }
    }
}

const DEFAULT_CAPACITY: usize = 4;

impl<T> Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena::new()
    }
}

impl<T> Arena<T> {
    /// Constructs a new, empty `Arena`.
    pub fn new() -> Arena<T> {
        Arena::with_capacity(DEFAULT_CAPACITY)
    }

    /// Constructs a new, empty `Arena<T>` with the specified capacity.
    pub fn with_capacity(n: usize) -> Arena<T> {
        let n = n.max(1);
        let mut arena = Arena {
            items: Vec::new(),
            generation: 0,
            free_list_head: None,
            len: 0,
        };
        arena.reserve(n);
        arena
    }

    /// Insert `value` into the arena, allocating more capacity if necessary.
    pub fn insert(&mut self, value: T) -> Index {
        match self.try_alloc_next_index() {
            Some(index) => {
                self.items[index.index as usize] = Entry::Occupied {
                    generation: self.generation,
                    value,
                };
                index
            }
            None => {
                let len = self.items.len();
                self.reserve(len);
                let index = self
                    .try_alloc_next_index()
                    .expect("inserting will always succeed after reserving additional space");
                self.items[index.index as usize] = Entry::Occupied {
                    generation: self.generation,
                    value,
                };
                index
            }
        }
    }

    fn try_alloc_next_index(&mut self) -> Option<Index> {
        match self.free_list_head {
            None => None,
            Some(i) => match self.items[i as usize] {
                Entry::Occupied { .. } => panic!("corrupt free list"),
                Entry::Free { next_free } => {
                    self.free_list_head = next_free;
                    self.len += 1;
                    Some(Index {
                        index: i,
                        generation: self.generation,
                    })
                }
            },
        }
    }

    /// Remove the element at index `i` from the arena.
    pub fn remove(&mut self, i: Index) -> Option<T> {
        if i.index as usize >= self.items.len() {
            return None;
        }

        match self.items[i.index as usize] {
            Entry::Occupied { generation, .. } if i.generation == generation => {
                let entry = mem::replace(
                    &mut self.items[i.index as usize],
                    Entry::Free {
                        next_free: self.free_list_head,
                    },
                );
                self.generation += 1;
                self.free_list_head = Some(i.index);
                self.len -= 1;

                match entry {
                    Entry::Occupied { value, .. } => Some(value),
                    _ => unreachable!(),
                }
            }
            _ => None,
        }
    }

    /// Get a shared reference to the element at index `i` if it is in the arena.
    pub fn get(&self, i: Index) -> Option<&T> {
        match self.items.get(i.index as usize) {
            Some(Entry::Occupied { generation, value }) if *generation == i.generation => {
                Some(value)
            }
            _ => None,
        }
    }

    /// Get an exclusive reference to the element at index `i` if it is in the arena.
    pub fn get_mut(&mut self, i: Index) -> Option<&mut T> {
        match self.items.get_mut(i.index as usize) {
            Some(Entry::Occupied { generation, value }) if *generation == i.generation => {
                Some(value)
            }
            _ => None,
        }
    }

    /// Get the length of this arena.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the arena contains no elements
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Get the capacity of this arena.
    pub fn capacity(&self) -> usize {
        self.items.len()
    }

    /// Allocate space for `additional_capacity` more elements in the arena.
    pub fn reserve(&mut self, additional_capacity: usize) {
        let start = self.items.len();
        let end = self.items.len() + additional_capacity;
        let old_head = self.free_list_head;
        self.items.reserve_exact(additional_capacity);
        self.items.extend((start..end).map(|i| {
            if i == end - 1 {
                Entry::Free {
                    next_free: old_head,
                }
            } else {
                Entry::Free {
                    next_free: Some((i + 1) as u32),
                }
            }
        }));
        self.free_list_head = Some(start as u32);
    }
}

impl<T> core::ops::Index<Index> for Arena<T> {
    type Output = T;

    fn index(&self, index: Index) -> &Self::Output {
        self.get(index).expect("No element at index")
    }
}

impl<T> core::ops::IndexMut<Index> for Arena<T> {
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        self.get_mut(index).expect("No element at index")
    }
}

// Iterator implementations for garbage collection

impl<T> Arena<T> {
    /// Iterate over shared references to the elements in this arena.
    /// Yields pairs of `(Index, &T)` items.
    pub fn iter(&self) -> Iter<T> {
        Iter {
            len: self.len,
            inner: self.items.iter().enumerate(),
        }
    }

    /// Iterate over exclusive references to the elements in this arena.
    /// Yields pairs of `(Index, &mut T)` items.
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut {
            len: self.len,
            inner: self.items.iter_mut().enumerate(),
        }
    }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type Item = (Index, &'a T);
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type Item = (Index, &'a mut T);
    type IntoIter = IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// An iterator over shared references to the elements in an arena.
/// Yields pairs of `(Index, &T)` items.
#[derive(Clone, Debug)]
pub struct Iter<'a, T: 'a> {
    len: usize,
    inner: core::iter::Enumerate<core::slice::Iter<'a, Entry<T>>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (Index, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                Some((_, &Entry::Free { .. })) => continue,
                Some((
                    index,
                    &Entry::Occupied {
                        generation,
                        ref value,
                    },
                )) => {
                    self.len -= 1;
                    let idx = Index {
                        index: index as u32,
                        generation,
                    };
                    return Some((idx, value));
                }
                None => {
                    debug_assert_eq!(self.len, 0);
                    return None;
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.len
    }
}

/// An iterator over exclusive references to elements in this arena.
/// Yields pairs of `(Index, &mut T)` items.
#[derive(Debug)]
pub struct IterMut<'a, T: 'a> {
    len: usize,
    inner: core::iter::Enumerate<core::slice::IterMut<'a, Entry<T>>>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = (Index, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                Some((_, &mut Entry::Free { .. })) => continue,
                Some((
                    index,
                    &mut Entry::Occupied {
                        generation,
                        ref mut value,
                    },
                )) => {
                    self.len -= 1;
                    let idx = Index {
                        index: index as u32,
                        generation,
                    };
                    return Some((idx, value));
                }
                None => {
                    debug_assert_eq!(self.len, 0);
                    return None;
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<'a, T> ExactSizeIterator for IterMut<'a, T> {
    fn len(&self) -> usize {
        self.len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_arena() {
        let arena: Arena<i32> = Arena::new();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
        assert_eq!(arena.capacity(), DEFAULT_CAPACITY);
    }

    #[test]
    fn test_with_capacity() {
        let arena: Arena<i32> = Arena::with_capacity(10);
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
        assert_eq!(arena.capacity(), 10);
    }

    #[test]
    fn test_with_capacity_zero() {
        let arena: Arena<i32> = Arena::with_capacity(0);
        assert_eq!(arena.capacity(), 1); // Should be at least 1
    }

    #[test]
    fn test_insert_and_get() {
        let mut arena = Arena::new();
        let idx1 = arena.insert(42);
        let idx2 = arena.insert(84);

        assert_eq!(arena.len(), 2);
        assert!(!arena.is_empty());
        assert_eq!(arena.get(idx1), Some(&42));
        assert_eq!(arena.get(idx2), Some(&84));
    }

    #[test]
    fn test_insert_and_get_mut() {
        let mut arena = Arena::new();
        let idx = arena.insert(42);

        if let Some(value) = arena.get_mut(idx) {
            *value = 100;
        }

        assert_eq!(arena.get(idx), Some(&100));
    }

    #[test]
    fn test_index_access() {
        let mut arena = Arena::new();
        let idx = arena.insert(42);

        assert_eq!(arena[idx], 42);
        arena[idx] = 100;
        assert_eq!(arena[idx], 100);
    }

    #[test]
    #[should_panic(expected = "No element at index")]
    fn test_index_access_invalid() {
        let arena: Arena<i32> = Arena::new();
        let invalid_idx = Index {
            index: 0,
            generation: 0,
        };
        let _ = arena[invalid_idx];
    }

    #[test]
    fn test_remove() {
        let mut arena = Arena::new();
        let idx1 = arena.insert(42);
        let idx2 = arena.insert(84);

        assert_eq!(arena.len(), 2);
        assert_eq!(arena.remove(idx1), Some(42));
        assert_eq!(arena.len(), 1);
        assert_eq!(arena.get(idx1), None);
        assert_eq!(arena.get(idx2), Some(&84));
    }

    #[test]
    fn test_remove_nonexistent() {
        let mut arena: Arena<i32> = Arena::new();
        let invalid_idx = Index {
            index: 0,
            generation: 0,
        };
        assert_eq!(arena.remove(invalid_idx), None);
    }

    #[test]
    fn test_remove_out_of_bounds() {
        let mut arena: Arena<i32> = Arena::new();
        let out_of_bounds_idx = Index {
            index: 100,
            generation: 0,
        };
        assert_eq!(arena.remove(out_of_bounds_idx), None);
    }

    #[test]
    fn test_generation_invalidation() {
        let mut arena = Arena::new();
        let idx = arena.insert(42);

        // Remove the element, which should increment generation
        assert_eq!(arena.remove(idx), Some(42));

        // The old index should no longer be valid
        assert_eq!(arena.get(idx), None);
        assert_eq!(arena.remove(idx), None);
    }

    #[test]
    fn test_reuse_freed_slots() {
        let mut arena = Arena::new();
        let idx1 = arena.insert(1);
        let idx2 = arena.insert(2);
        let idx3 = arena.insert(3);

        // Remove middle element
        arena.remove(idx2);

        // Insert new element, should reuse the freed slot
        let idx4 = arena.insert(4);

        // The new element should be at the same index but with different generation
        assert_eq!(idx4.index, idx2.index);
        assert_ne!(idx4.generation, idx2.generation);

        assert_eq!(arena.get(idx1), Some(&1));
        assert_eq!(arena.get(idx2), None); // Old reference invalid
        assert_eq!(arena.get(idx3), Some(&3));
        assert_eq!(arena.get(idx4), Some(&4));
    }

    #[test]
    fn test_capacity_growth() {
        let mut arena = Arena::with_capacity(2);
        assert_eq!(arena.capacity(), 2);

        arena.insert(1);
        arena.insert(2);
        assert_eq!(arena.capacity(), 2);

        // This should trigger capacity growth
        arena.insert(3);
        assert!(arena.capacity() > 2);
    }

    #[test]
    fn test_reserve() {
        let mut arena: Arena<i32> = Arena::with_capacity(2);
        assert_eq!(arena.capacity(), 2);

        arena.reserve(5);
        assert_eq!(arena.capacity(), 7); // 2 + 5
    }

    #[test]
    fn test_default() {
        let arena: Arena<i32> = Arena::default();
        assert_eq!(arena.len(), 0);
        assert!(arena.is_empty());
        assert_eq!(arena.capacity(), DEFAULT_CAPACITY);
    }

    #[test]
    fn test_index_default() {
        let idx = Index::default();
        assert_eq!(idx.index, u32::MAX);
        assert_eq!(idx.generation, u32::MAX);
    }

    #[test]
    fn test_iter() {
        let mut arena = Arena::new();
        let idx1 = arena.insert(10);
        let idx2 = arena.insert(20);
        let idx3 = arena.insert(30);

        let mut items: Vec<_> = arena.iter().collect();
        items.sort_by_key(|(_, val)| *val);

        assert_eq!(items.len(), 3);
        assert_eq!(items[0], (idx1, &10));
        assert_eq!(items[1], (idx2, &20));
        assert_eq!(items[2], (idx3, &30));
    }

    #[test]
    fn test_iter_mut() {
        let mut arena = Arena::new();
        arena.insert(10);
        arena.insert(20);
        arena.insert(30);

        for (_, value) in arena.iter_mut() {
            *value *= 2;
        }

        let values: Vec<_> = arena.iter().map(|(_, &val)| val).collect();
        assert!(values.contains(&20));
        assert!(values.contains(&40));
        assert!(values.contains(&60));
    }

    #[test]
    fn test_iter_with_removed_elements() {
        let mut arena = Arena::new();
        let _idx1 = arena.insert(10);
        let idx2 = arena.insert(20);
        let _idx3 = arena.insert(30);

        arena.remove(idx2);

        let items: Vec<_> = arena.iter().collect();
        assert_eq!(items.len(), 2);

        let values: Vec<_> = items.iter().map(|(_, val)| **val).collect();
        assert!(values.contains(&10));
        assert!(values.contains(&30));
        assert!(!values.contains(&20));
    }

    #[test]
    fn test_into_iter_ref() {
        let mut arena = Arena::new();
        arena.insert(1);
        arena.insert(2);
        arena.insert(3);

        let count = (&arena).into_iter().count();
        assert_eq!(count, 3);
    }

    #[test]
    fn test_into_iter_mut() {
        let mut arena = Arena::new();
        arena.insert(1);
        arena.insert(2);
        arena.insert(3);

        for (_, value) in &mut arena {
            *value += 10;
        }

        let values: Vec<_> = arena.iter().map(|(_, &val)| val).collect();
        assert!(values.contains(&11));
        assert!(values.contains(&12));
        assert!(values.contains(&13));
    }

    #[test]
    fn test_iter_size_hint() {
        let mut arena = Arena::new();
        arena.insert(1);
        arena.insert(2);
        arena.insert(3);

        let iter = arena.iter();
        assert_eq!(iter.size_hint(), (3, Some(3)));
        assert_eq!(iter.len(), 3);
    }

    #[test]
    fn test_iter_exact_size() {
        let mut arena = Arena::new();
        arena.insert(1);
        arena.insert(2);

        let iter = arena.iter();
        assert_eq!(iter.len(), 2);

        let iter_mut = arena.iter_mut();
        assert_eq!(iter_mut.len(), 2);
    }

    #[test]
    fn test_empty_iter() {
        let arena: Arena<i32> = Arena::new();
        let items: Vec<_> = arena.iter().collect();
        assert!(items.is_empty());

        let iter = arena.iter();
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.len(), 0);
    }

    #[test]
    fn test_generation_overflow_safety() {
        let mut arena = Arena::new();
        let idx = arena.insert(42);

        // Simulate many removes to test generation handling
        for _ in 0..10 {
            arena.remove(idx);
            arena.insert(1);
        }

        // Original index should still be invalid
        assert_eq!(arena.get(idx), None);
    }

    #[test]
    fn test_multiple_remove_same_index() {
        let mut arena = Arena::new();
        let idx = arena.insert(42);

        assert_eq!(arena.remove(idx), Some(42));
        assert_eq!(arena.remove(idx), None);
        assert_eq!(arena.remove(idx), None);
    }

    #[test]
    fn test_stress_insert_remove() {
        let mut arena = Arena::new();
        let mut indices = Vec::new();

        // Insert many elements
        for i in 0..100 {
            indices.push(arena.insert(i));
        }
        assert_eq!(arena.len(), 100);

        // Remove every other element
        for i in (0..indices.len()).step_by(2) {
            arena.remove(indices[i]);
        }
        assert_eq!(arena.len(), 50);

        // Insert more elements (should reuse freed slots)
        for i in 100..150 {
            arena.insert(i);
        }
        assert_eq!(arena.len(), 100);
    }

    #[test]
    fn test_clone() {
        let mut arena = Arena::new();
        let idx1 = arena.insert(42);
        let idx2 = arena.insert(84);

        let cloned = arena.clone();

        assert_eq!(cloned.len(), arena.len());
        assert_eq!(cloned.get(idx1), arena.get(idx1));
        assert_eq!(cloned.get(idx2), arena.get(idx2));
    }

    #[test]
    fn test_debug_formatting() {
        let mut arena = Arena::new();
        arena.insert(42);

        let debug_str = format!("{:?}", arena);
        assert!(debug_str.contains("Arena"));

        let idx = Index {
            index: 0,
            generation: 0,
        };
        let debug_idx = format!("{:?}", idx);
        assert!(debug_idx.contains("Index"));
    }

    #[test]
    fn test_index_equality_and_ordering() {
        let idx1 = Index {
            index: 0,
            generation: 0,
        };
        let idx2 = Index {
            index: 0,
            generation: 0,
        };
        let idx3 = Index {
            index: 1,
            generation: 0,
        };
        let idx4 = Index {
            index: 0,
            generation: 1,
        };

        assert_eq!(idx1, idx2);
        assert_ne!(idx1, idx3);
        assert_ne!(idx1, idx4);

        assert!(idx1 < idx3);
        assert!(idx1 < idx4);
    }

    #[test]
    fn test_large_capacity() {
        let mut arena = Arena::with_capacity(1000);
        assert_eq!(arena.capacity(), 1000);

        for i in 0..500 {
            arena.insert(i);
        }
        assert_eq!(arena.len(), 500);
        assert_eq!(arena.capacity(), 1000);
    }
}
