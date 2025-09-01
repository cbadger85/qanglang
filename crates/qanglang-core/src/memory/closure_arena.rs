use crate::{ClosureObject, debug_log};
use crate::{
    arena::{Arena, Index},
    memory::UpvalueHandle,
};

pub const INLINE_UPVALUE_COUNT: usize = 4;
const OVERFLOW_CHUNK_SIZE: usize = 8;

pub type ClosureHandle = Index;
pub type OverflowHandle = Index;

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum UpvalueSlot {
    Open(usize),
    Closed(UpvalueHandle),
}

impl Default for UpvalueSlot {
    fn default() -> Self {
        Self::Open(0)
    }
}

#[derive(Debug, Clone)]
pub struct OverflowChunk {
    pub entries: [UpvalueSlot; OVERFLOW_CHUNK_SIZE],
    pub count: usize,
    pub next: Option<OverflowHandle>,
    pub is_marked: bool,
}

impl Default for OverflowChunk {
    fn default() -> Self {
        Self {
            entries: [UpvalueSlot::default(); OVERFLOW_CHUNK_SIZE],
            count: 0,
            next: None,
            is_marked: false,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ClosureArena {
    closures: Arena<ClosureObject>,
    overflow_chunks: Arena<OverflowChunk>,
    is_debug: bool,
}

impl ClosureArena {
    pub fn new() -> Self {
        Self::with_capacity(32)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            closures: Arena::with_capacity(capacity),
            overflow_chunks: Arena::with_capacity(capacity),
            is_debug: false,
        }
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let handle = self.closures.insert(closure);
        debug_log!(
            self.is_debug,
            "Allocated {} bytes for closure: {:?}",
            std::mem::size_of::<ClosureObject>(),
            handle
        );
        handle
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        self.closures.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} bytes for closure: {:?}",
            std::mem::size_of::<ClosureObject>(),
            handle
        );
    }

    pub fn get_upvalue(&self, closure_handle: ClosureHandle, index: usize) -> Option<UpvalueSlot> {
        let closure = self.closures.get(closure_handle)?;

        if index >= closure.upvalue_count {
            return None;
        }

        if index < INLINE_UPVALUE_COUNT {
            Some(closure.inline_upvalues[index])
        } else {
            let mut overflow_index = index - INLINE_UPVALUE_COUNT;
            let mut current_handle = closure.overflow_handle?;

            loop {
                let chunk = self.get_overflow_chunk(current_handle)?;
                if overflow_index < chunk.count {
                    return Some(chunk.entries[overflow_index]);
                }
                overflow_index -= chunk.count;
                current_handle = chunk.next?;
            }
        }
    }

    pub fn set_upvalue(
        &mut self,
        closure_handle: ClosureHandle,
        index: usize,
        value: UpvalueSlot,
    ) -> bool {
        let upvalue_count = if let Some(closure) = self.closures.get(closure_handle) {
            closure.upvalue_count
        } else {
            return false;
        };

        if index >= upvalue_count {
            return false;
        }

        if index < INLINE_UPVALUE_COUNT {
            if let Some(closure) = self.closures.get_mut(closure_handle) {
                closure.inline_upvalues[index] = value;
                true
            } else {
                false
            }
        } else {
            let mut overflow_index = index - INLINE_UPVALUE_COUNT;

            // Ensure overflow chain exists
            let first_handle = if let Some(closure) = self.closures.get(closure_handle) {
                if let Some(handle) = closure.overflow_handle {
                    handle
                } else {
                    let handle = self.allocate_overflow_chunk();
                    if let Some(closure) = self.closures.get_mut(closure_handle) {
                        closure.overflow_handle = Some(handle);
                    }
                    handle
                }
            } else {
                return false;
            };

            let mut current_handle = first_handle;
            loop {
                if overflow_index < OVERFLOW_CHUNK_SIZE {
                    if let Some(chunk) = self.get_overflow_chunk_mut(current_handle) {
                        chunk.entries[overflow_index] = value;
                        chunk.count = chunk.count.max(overflow_index + 1);
                        return true;
                    } else {
                        return false;
                    }
                }

                overflow_index -= OVERFLOW_CHUNK_SIZE;

                let next_handle = if let Some(chunk) = self.get_overflow_chunk(current_handle) {
                    if let Some(next) = chunk.next {
                        next
                    } else {
                        let new_handle = self.allocate_overflow_chunk();
                        if let Some(chunk) = self.get_overflow_chunk_mut(current_handle) {
                            chunk.next = Some(new_handle);
                        }
                        new_handle
                    }
                } else {
                    return false;
                };

                current_handle = next_handle;
            }
        }
    }

    pub fn allocate_overflow_chunk(&mut self) -> OverflowHandle {
        self.overflow_chunks.insert(OverflowChunk::default())
    }

    pub fn get_overflow_chunk(&self, handle: OverflowHandle) -> Option<&OverflowChunk> {
        self.overflow_chunks.get(handle)
    }

    pub fn get_overflow_chunk_mut(&mut self, handle: OverflowHandle) -> Option<&mut OverflowChunk> {
        self.overflow_chunks.get_mut(handle)
    }

    pub fn mark_closure(&mut self, handle: ClosureHandle) {
        if let Some(closure) = self.closures.get_mut(handle) {
            closure.is_marked = true;
            if let Some(overflow_handle) = closure.overflow_handle {
                self.mark_overflow_chain(overflow_handle);
            }
        }
    }

    pub fn mark_overflow_chain(&mut self, handle: OverflowHandle) {
        let mut current_handle = Some(handle);
        while let Some(h) = current_handle {
            if let Some(chunk) = self.overflow_chunks.get_mut(h) {
                if chunk.is_marked {
                    break;
                }
                chunk.is_marked = true;
                current_handle = chunk.next;
            } else {
                break;
            }
        }
    }

    pub fn trace_closure_references(
        &mut self,
        handle: ClosureHandle,
        upvalue_tracer: &mut dyn FnMut(UpvalueHandle),
    ) {
        let (should_mark, overflow_handle, upvalue_count) =
            if let Some(closure) = self.closures.get(handle) {
                (
                    !closure.is_marked,
                    closure.overflow_handle,
                    closure.upvalue_count,
                )
            } else {
                return;
            };

        if should_mark {
            debug_log!(self.is_debug, "Marking closure: {:?}", handle);
            if let Some(closure) = self.closures.get_mut(handle) {
                closure.is_marked = true;
            }

            if let Some(overflow_handle) = overflow_handle {
                self.mark_overflow_chain(overflow_handle);
            }

            debug_log!(self.is_debug, "Blackening closure: {:?}", handle);

            for i in 0..upvalue_count {
                if let Some(UpvalueSlot::Closed(upvalue_handle)) = self.get_upvalue(handle, i) {
                    upvalue_tracer(upvalue_handle);
                }
            }
        }
    }

    pub fn collect_garbage(&mut self) {
        let estimated_deletions = (self.closures.len() / 4).max(8);
        let mut handles_to_remove = Vec::with_capacity(estimated_deletions);

        for (handle, closure) in self.closures.iter_mut() {
            if closure.is_marked {
                closure.is_marked = false;
            } else {
                handles_to_remove.push(handle);
            }
        }

        for handle in &handles_to_remove {
            self.closures.remove(*handle);
        }

        handles_to_remove.clear();

        for (handle, chunk) in self.overflow_chunks.iter_mut() {
            if chunk.is_marked {
                chunk.is_marked = false;
            } else {
                handles_to_remove.push(handle);
            }
        }

        for handle in &handles_to_remove {
            self.overflow_chunks.remove(*handle);
        }
    }

    pub fn get_allocated_bytes(&self) -> usize {
        let closure_bytes = self.closures.len() * std::mem::size_of::<ClosureObject>();
        let overflow_bytes = self.overflow_chunks.len() * std::mem::size_of::<OverflowChunk>();
        closure_bytes + overflow_bytes
    }

    pub fn len(&self) -> usize {
        self.closures.len()
    }

    pub fn is_empty(&self) -> bool {
        self.closures.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClosureHandle, &ClosureObject)> {
        self.closures.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ClosureHandle, &mut ClosureObject)> {
        self.closures.iter_mut()
    }
}
