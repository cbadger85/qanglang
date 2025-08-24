use crate::{
    Value,
    memory::{Arena, Index},
};

const CHUNK_SIZE: usize = 32;

pub type ArrayHandle = Index;
pub type ChunkHandle = Index;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArrayHeader {
    first_chunk: Option<ChunkHandle>,
    length: usize,
    chunks_count: usize,
    is_marked: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayChunk {
    data: [Option<Value>; CHUNK_SIZE],
    next_chunk: Option<ChunkHandle>,
    used: usize, // how many slots are actually used
    is_marked: bool,
}

pub struct ArrayArena {
    heads: Arena<ArrayHeader>,
    chunks: Arena<ArrayChunk>,
}

impl ArrayArena {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            heads: Arena::with_capacity(capacity),
            chunks: Arena::with_capacity(capacity),
        }
    }

    pub fn create_array(length: usize) -> ArrayHandle {
        // TODO creates a new array of the given size. All fields should remain null.
    }

    pub fn insert(handle: ArrayHandle, index: usize, value: Value) -> bool {
        // TODO inserts an element at a given index. if the index is out of bounds, return false and do not make an insertion.
    }

    pub fn get(handle: ArrayHandle, index: usize) -> Value {
        // TODO inserts an element at a given index. if the index is out of bounds, return false and do not make an insertion.
    }

    pub fn pop(handle: ArrayHandle) -> Value {
        // TODO pop the last item off the array. If this removes the last element from a chunk, the chunk should be marked for gc.
    }

    pub fn push(handle: ArrayHandle) -> Value {
        // TODO push a new value on to the array.
    }

    pub fn reverse(handle: ArrayHandle) {
        // TODO reverse the array, preferably in place and without any heap allocations.
    }

    pub fn length(handle: ArrayHandle) -> usize {
        // TODO returns the length of an array.
    }

    pub fn concat(handle1: ArrayHandle, handle2: ArrayHandle) -> ArrayHandle {

        // TODO concats two arrays together. Preferably this should happen without heap allocations. The below might be useful, but arrays should be preferred over Vec. Vec is acceptable if the size is not known at compile time but when using it at runtime, it should always be initialized with capacity.

        /*
         Rust enforces that there can only be one mutable reference with no immutable references to a particular piece of data in a particular scope. Because of this, attempting to use copy_from_slice on a single slice will result in a compile failure:

         let mut slice = [1, 2, 3, 4, 5];

         slice[..2].copy_from_slice(&slice[3..]); // compile fail!
         To work around this, we can use split_at_mut to create two distinct sub-slices from a slice:

         let mut slice = [1, 2, 3, 4, 5];

         {
             let (left, right) = slice.split_at_mut(2);
             left.copy_from_slice(&right[1..]);
         }
        */
    }

    pub fn slice(handle: ArrayHandle, begin: usize, end: usize) -> ArrayHandle {
        // TODO creates a new array from a slice of an existing one.
    }
}
