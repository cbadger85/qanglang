use std::{cell::RefCell, rc::Rc};

use crate::{Value, chunk::Chunk, memory::StringHandle};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ClosureObject {
    pub function: Rc<FunctionObject>,
    pub upvalue_count: usize,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

impl ClosureObject {
    pub fn new(function: Rc<FunctionObject>) -> Self {
        let upvalue_count = function.upvalue_count;
        let upvalues = vec![Rc::new(RefCell::new(Upvalue::Open(0))); upvalue_count];
        Self {
            function,
            upvalues,
            upvalue_count,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
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
