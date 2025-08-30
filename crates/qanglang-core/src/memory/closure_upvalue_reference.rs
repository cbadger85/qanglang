use crate::memory::{ClosureHandle, arena::Index};

pub type UpvalueOverflowHandle = Index;

const INLINE_UPVALUE_CAPACITY: usize = 4;

#[derive(Debug, Clone)]
pub struct ClosureUpvalueReference {
    inline_entries: [(ClosureHandle, usize); INLINE_UPVALUE_CAPACITY],
    count: usize,
    overflow_handle: Option<UpvalueOverflowHandle>,
}

impl Default for ClosureUpvalueReference {
    fn default() -> Self {
        Self {
            inline_entries: [(ClosureHandle::default(), 0); INLINE_UPVALUE_CAPACITY],
            count: 0,
            overflow_handle: None,
        }
    }
}

impl ClosureUpvalueReference {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_closure(&mut self, closure_handle: ClosureHandle, upvalue_index: usize) -> bool {
        if self.count < INLINE_UPVALUE_CAPACITY {
            self.inline_entries[self.count] = (closure_handle, upvalue_index);
            self.count += 1;
            true
        } else {
            false // Need overflow allocation
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClosureHandle, usize)> + '_ {
        self.inline_entries[..self.count].iter().copied()
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
