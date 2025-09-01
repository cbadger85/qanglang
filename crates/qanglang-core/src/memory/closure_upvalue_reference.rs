use crate::memory::{ClosureHandle, arena::Index};

pub type UpvalueOverflowHandle = Index;

const INLINE_UPVALUE_CAPACITY: usize = 4;

#[derive(Debug, Clone)]
pub struct OpenUpvalueTracker {
    inline_entries: [(ClosureHandle, usize); INLINE_UPVALUE_CAPACITY],
    count: usize,
    overflow_handle: Option<UpvalueOverflowHandle>,
}

impl Default for OpenUpvalueTracker {
    fn default() -> Self {
        Self {
            inline_entries: [(ClosureHandle::default(), 0); INLINE_UPVALUE_CAPACITY],
            count: 0,
            overflow_handle: None,
        }
    }
}

impl OpenUpvalueTracker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_closure(
        &mut self,
        closure_handle: ClosureHandle,
        upvalue_index: usize,
        overflow_arena: &mut crate::memory::UpvalueOverflowArena,
    ) -> bool {
        if self.count < INLINE_UPVALUE_CAPACITY {
            self.inline_entries[self.count] = (closure_handle, upvalue_index);
            self.count += 1;
            return true;
        }

        // Need overflow storage
        if self.overflow_handle.is_none() {
            let handle = overflow_arena.allocate_chunk();
            self.overflow_handle = Some(handle);
        }

        // Find the last chunk in the chain
        let mut current_handle = self.overflow_handle.unwrap();
        loop {
            {
                let chunk = overflow_arena.get_chunk_mut(current_handle);
                if chunk.count < crate::memory::upvalue_overflow_arena::OVERFLOW_CHUNK_SIZE {
                    chunk.entries[chunk.count] = (closure_handle, upvalue_index);
                    chunk.count += 1;
                    return true;
                }
            }

            let next_handle = {
                let chunk = overflow_arena.get_chunk(current_handle);
                chunk.next
            };

            if let Some(next) = next_handle {
                current_handle = next;
            } else {
                // Last chunk is full, create a new one
                let new_handle = overflow_arena.allocate_chunk();
                overflow_arena.get_chunk_mut(current_handle).next = Some(new_handle);
                let new_chunk = overflow_arena.get_chunk_mut(new_handle);
                new_chunk.entries[0] = (closure_handle, upvalue_index);
                new_chunk.count = 1;
                return true;
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClosureHandle, usize)> + '_ {
        self.inline_entries[..self.count].iter().copied()
    }

    pub fn iter_all_entries<'a>(
        &'a self,
        overflow_arena: &'a crate::memory::UpvalueOverflowArena,
    ) -> impl Iterator<Item = (ClosureHandle, usize)> + 'a {
        UpvalueEntryIterator::new(self, overflow_arena)
    }

    pub fn overflow_handle(&self) -> Option<UpvalueOverflowHandle> {
        self.overflow_handle
    }

    pub fn set_overflow_handle(&mut self, handle: UpvalueOverflowHandle) {
        self.overflow_handle = Some(handle);
    }

    pub fn len(&self, overflow_arena: &crate::memory::UpvalueOverflowArena) -> usize {
        let mut total = self.count;

        if let Some(mut current_handle) = self.overflow_handle {
            loop {
                let chunk = overflow_arena.get_chunk(current_handle);
                total += chunk.count;
                if let Some(next_handle) = chunk.next {
                    current_handle = next_handle;
                } else {
                    break;
                }
            }
        }

        total
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn mark_overflow_chunks(&self, overflow_arena: &mut crate::memory::UpvalueOverflowArena) {
        if let Some(mut current_handle) = self.overflow_handle {
            loop {
                overflow_arena.mark_chunk(current_handle);
                let chunk = overflow_arena.get_chunk(current_handle);
                if let Some(next_handle) = chunk.next {
                    current_handle = next_handle;
                } else {
                    break;
                }
            }
        }
    }
}

struct UpvalueEntryIterator<'a> {
    tracker: &'a OpenUpvalueTracker,
    overflow_arena: &'a crate::memory::UpvalueOverflowArena,
    inline_index: usize,
    current_chunk: Option<UpvalueOverflowHandle>,
    chunk_index: usize,
}

impl<'a> UpvalueEntryIterator<'a> {
    fn new(
        tracker: &'a OpenUpvalueTracker,
        overflow_arena: &'a crate::memory::UpvalueOverflowArena,
    ) -> Self {
        Self {
            tracker,
            overflow_arena,
            inline_index: 0,
            current_chunk: tracker.overflow_handle,
            chunk_index: 0,
        }
    }
}

impl<'a> Iterator for UpvalueEntryIterator<'a> {
    type Item = (ClosureHandle, usize);

    fn next(&mut self) -> Option<Self::Item> {
        // First, iterate through inline entries
        if self.inline_index < self.tracker.count {
            let entry = self.tracker.inline_entries[self.inline_index];
            self.inline_index += 1;
            return Some(entry);
        }

        // Then iterate through overflow chunks
        while let Some(chunk_handle) = self.current_chunk {
            let chunk = self.overflow_arena.get_chunk(chunk_handle);

            if self.chunk_index < chunk.count {
                let entry = chunk.entries[self.chunk_index];
                self.chunk_index += 1;
                return Some(entry);
            }

            // Move to next chunk
            self.current_chunk = chunk.next;
            self.chunk_index = 0;
        }

        None
    }
}
