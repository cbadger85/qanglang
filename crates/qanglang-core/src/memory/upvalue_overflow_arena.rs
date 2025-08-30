use crate::memory::arena::{Arena, Index};
use crate::memory::ClosureHandle;

const OVERFLOW_CHUNK_SIZE: usize = 8;

pub type UpvalueOverflowHandle = Index;

#[derive(Debug, Clone)]
pub struct UpvalueOverflowChunk {
    pub entries: [(ClosureHandle, usize); OVERFLOW_CHUNK_SIZE],
    pub count: usize,
    pub is_marked: bool,
}

impl Default for UpvalueOverflowChunk {
    fn default() -> Self {
        Self {
            entries: [(ClosureHandle::default(), 0); OVERFLOW_CHUNK_SIZE],
            count: 0,
            is_marked: false,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct UpvalueOverflowArena {
    chunks: Arena<UpvalueOverflowChunk>,
}

impl UpvalueOverflowArena {
    pub fn new() -> Self {
        Self::with_capacity(32)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            chunks: Arena::with_capacity(capacity),
        }
    }

    pub fn allocate_chunk(&mut self) -> UpvalueOverflowHandle {
        self.chunks.insert(UpvalueOverflowChunk::default())
    }

    pub fn get_chunk(&self, handle: UpvalueOverflowHandle) -> &UpvalueOverflowChunk {
        &self.chunks[handle]
    }

    pub fn get_chunk_mut(&mut self, handle: UpvalueOverflowHandle) -> &mut UpvalueOverflowChunk {
        &mut self.chunks[handle]
    }

    pub fn mark_chunk(&mut self, handle: UpvalueOverflowHandle) {
        if let Some(chunk) = self.chunks.get_mut(handle) {
            chunk.is_marked = true;
        }
    }

    pub fn collect_garbage(&mut self) {
        let estimated_deletions = (self.chunks.len() / 4).max(8);
        let mut handles_to_remove = Vec::with_capacity(estimated_deletions);

        for (handle, chunk) in self.chunks.iter_mut() {
            if chunk.is_marked {
                chunk.is_marked = false;
            } else {
                handles_to_remove.push(handle);
            }
        }

        for handle in handles_to_remove {
            self.chunks.remove(handle);
        }
    }

    pub fn get_allocated_bytes(&self) -> usize {
        self.chunks.len() * std::mem::size_of::<UpvalueOverflowChunk>()
    }
}