use std::{cell::RefCell, rc::Rc};

use crate::{ObjectHandle, Value, chunk::Chunk, error::ValueConversionError};

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

impl From<Rc<ClosureObject>> for QangObject {
    fn from(value: Rc<ClosureObject>) -> Self {
        QangObject::Closure(value)
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
    pub name: ObjectHandle,
    pub chunk: Chunk,
    pub upvalue_count: usize,
}

impl FunctionObject {
    pub fn new(name: ObjectHandle, arity: usize) -> Self {
        Self {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }
}

impl From<FunctionObject> for QangObject {
    fn from(value: FunctionObject) -> Self {
        QangObject::Function(Rc::new(value))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum QangObject {
    String(Box<str>),
    Function(Rc<FunctionObject>),
    Closure(Rc<ClosureObject>),
}

impl TryFrom<QangObject> for Box<str> {
    type Error = ValueConversionError;

    fn try_from(value: QangObject) -> Result<Self, Self::Error> {
        match value {
            QangObject::String(string) => Ok(string),
            _ => Err(ValueConversionError::new(
                format!("Expected string, found {}.", get_object_value_type(&value)).as_str(),
            )),
        }
    }
}

pub const fn get_object_value_type(value: &QangObject) -> &'static str {
    match value {
        QangObject::String(_) => "string",
        QangObject::Closure(_) | QangObject::Function(_) => "function",
    }
}
