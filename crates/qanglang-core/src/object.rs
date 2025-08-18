use generational_arena::Index;

use crate::{
    Value,
    chunk::Chunk,
    memory::{FunctionHandle, StringHandle, ValueHandle},
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionHandle,
    pub upvalue_count: usize,
    pub upvalues: Vec<UpvalueReference>,
}

impl ClosureObject {
    pub fn new(function: FunctionHandle, upvalue_count: usize) -> Self {
        let upvalues = vec![UpvalueReference::Open(0); upvalue_count];
        Self {
            function,
            upvalues,
            upvalue_count,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UpvalueReference {
    Open(usize),
    Closed(ValueHandle),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FunctionObject {
    pub arity: usize,
    pub name: StringHandle,
    pub chunk: Chunk,
    pub upvalue_count: usize,
}

impl FunctionObject {
    pub fn new(name: StringHandle, arity: usize) -> Self {
        Self {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }
}

// Idea for array implementation
const CHUNK_SIZE: usize = 32;

// This becomes a Value.
#[derive(Debug, Clone, Copy, PartialEq)]

pub struct ArrayHeader {
    first_chunk: Option<Index>,
    length: usize,
    chunks_count: usize,
}

// This get's an arena allocator for it.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayChunk {
    data: [Option<Value>; CHUNK_SIZE],
    next_chunk: Option<Index>,
    used: usize, // how many slots are actually used
}
