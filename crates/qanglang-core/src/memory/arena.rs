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
