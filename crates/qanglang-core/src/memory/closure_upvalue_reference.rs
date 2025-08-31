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

    pub fn add_closure(&mut self, closure_handle: ClosureHandle, upvalue_index: usize) -> bool {
        if self.count < INLINE_UPVALUE_CAPACITY {
            self.inline_entries[self.count] = (closure_handle, upvalue_index);
            self.count += 1;
            true
        } else {
            false // Need overflow allocation - caller must handle this
        }
    }

    pub fn add_closure_overflow(
        &mut self,
        closure_handle: ClosureHandle,
        upvalue_index: usize,
        overflow_arena: &mut crate::memory::UpvalueOverflowArena,
    ) -> bool {
        // First try inline
        if self.add_closure(closure_handle, upvalue_index) {
            return true;
        }

        // Need overflow storage
        if self.overflow_handle.is_none() {
            // Allocate new overflow chunk
            let handle = overflow_arena.allocate_chunk();
            self.overflow_handle = Some(handle);
        }

        // Add to overflow chunk
        if let Some(overflow_handle) = self.overflow_handle {
            let chunk = overflow_arena.get_chunk_mut(overflow_handle);
            if chunk.count < crate::memory::upvalue_overflow_arena::OVERFLOW_CHUNK_SIZE {
                chunk.entries[chunk.count] = (closure_handle, upvalue_index);
                chunk.count += 1;
                true
            } else {
                // Overflow chunk is full - this shouldn't happen with reasonable limits
                false
            }
        } else {
            false
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClosureHandle, usize)> + '_ {
        self.inline_entries[..self.count].iter().copied()
    }

    pub fn collect_all_entries(
        &self,
        overflow_arena: &crate::memory::UpvalueOverflowArena,
    ) -> Vec<(ClosureHandle, usize)> {
        let mut entries = Vec::new();
        
        // Add inline entries
        entries.extend_from_slice(&self.inline_entries[..self.count]);
        
        // Add overflow entries if they exist
        if let Some(overflow_handle) = self.overflow_handle {
            let chunk = overflow_arena.get_chunk(overflow_handle);
            entries.extend_from_slice(&chunk.entries[..chunk.count]);
        }
        
        entries
    }

    pub fn overflow_handle(&self) -> Option<UpvalueOverflowHandle> {
        self.overflow_handle
    }

    pub fn set_overflow_handle(&mut self, handle: UpvalueOverflowHandle) {
        self.overflow_handle = Some(handle);
    }

    pub fn len(&self) -> usize {
        self.count
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }
}
