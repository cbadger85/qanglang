use crate::{
    Value,
    chunk::Chunk,
    memory::{FunctionHandle, StringHandle, UpvalueHandle},
};

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionHandle,
    pub upvalue_count: usize,
    pub upvalues: [UpvalueReference; 256],
    pub is_marked: bool,
}

impl ClosureObject {
    pub fn new(function: FunctionHandle, upvalue_count: usize) -> Self {
        let upvalues = std::array::from_fn(|_| UpvalueReference::default());
        Self {
            function,
            upvalues,
            upvalue_count,
            is_marked: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum UpvalueReference {
    Open(usize),
    Closed(UpvalueHandle),
}

impl Default for UpvalueReference {
    fn default() -> Self {
        Self::Open(0)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Upvalue {
    pub value: Value,
    pub is_marked: bool,
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
