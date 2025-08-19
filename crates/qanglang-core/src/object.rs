use crate::{
    Value,
    chunk::Chunk,
    memory::{FunctionHandle, StringHandle, UpvalueHandle},
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionHandle,
    pub upvalue_count: usize,
    pub upvalues: Vec<UpvalueReference>,
    pub is_marked: bool,
}

impl ClosureObject {
    pub fn new(function: FunctionHandle, upvalue_count: usize) -> Self {
        let upvalues = vec![UpvalueReference::Open(0); upvalue_count];
        Self {
            function,
            upvalues,
            upvalue_count,
            is_marked: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UpvalueReference {
    Open(usize),
    Closed(UpvalueHandle),
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
