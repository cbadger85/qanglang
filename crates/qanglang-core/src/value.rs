use crate::{
    NativeFn, ObjectHeap,
    error::ValueConversionError,
    memory::{ClosureHandle, FunctionHandle, StringHandle},
};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Copy, Hash)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub arity: usize,
    pub name_handle: StringHandle,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name_handle == other.name_handle
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum FunctionValueKind {
    Closure(ClosureHandle),
    NativeFunction(NativeFunction),
}

pub const NIL_TYPE_STRING: &str = "nil";
pub const BOOLEAN_TYPE_STRING: &str = "boolean";
pub const NUMBER_TYPE_STRING: &str = "number";
pub const STRING_TYPE_STRING: &str = "string";
pub const FUNCTION_TYPE_STRING: &str = "function";

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(StringHandle),
    Function(FunctionValueKind),
    FunctionDecl(FunctionHandle),
}

impl Value {
    pub fn print(&self, heap: &ObjectHeap) {
        print!("{}", self.to_display_string(heap));
    }

    pub fn to_display_string(&self, heap: &ObjectHeap) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(number) => number.to_string(),
            Value::String(handle) => heap.get_string(*handle).to_string(),
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::FunctionDecl(function_handle) => {
                let function = heap.get_function(*function_handle);
                let identifier = heap.get_string(function.name);
                format!("<function>{}", identifier)
            }
            Value::Function(function) => match function {
                FunctionValueKind::NativeFunction(function) => {
                    let identifier = heap.get_string(function.name_handle);
                    format!("<function>{}", identifier)
                }
                FunctionValueKind::Closure(handle) => {
                    let closure = heap.get_closure(*handle);
                    let function = heap.get_function(closure.function);
                    let identifier = heap.get_string(function.name);
                    format!("<function>{}", identifier)
                }
            },
        }
    }

    pub const fn to_type_string(&self) -> &'static str {
        match self {
            Value::Nil => NIL_TYPE_STRING,
            Value::True => BOOLEAN_TYPE_STRING,
            Value::False => BOOLEAN_TYPE_STRING,
            Value::Number(_) => NUMBER_TYPE_STRING,
            Value::String(_) => STRING_TYPE_STRING,
            Value::Function(_) => FUNCTION_TYPE_STRING,
            Value::FunctionDecl(_) => FUNCTION_TYPE_STRING,
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::False)
    }

    pub fn hash(&self) -> u64 {
        let mut hasher = FxHasher::default();
        match self {
            Value::Nil => 0u8.hash(&mut hasher),
            Value::True => true.hash(&mut hasher),
            Value::False => false.hash(&mut hasher),
            Value::Number(n) => n.to_bits().hash(&mut hasher),
            Value::String(handle) => handle.hash(&mut hasher),
            Value::Function(kind) => kind.hash(&mut hasher),
            Value::FunctionDecl(handle) => handle.hash(&mut hasher),
        }
        hasher.finish()
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::Number(num)
    }
}

impl From<u64> for Value {
    fn from(num: u64) -> Self {
        Value::Number(num as f64)
    }
}

impl TryFrom<Value> for f64 {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(number) => Ok(number),
            _ => Err(ValueConversionError::new(
                format!("Expected number, found {}.", value.to_type_string()).as_str(),
            )),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::True => Ok(true),
            Value::False => Ok(false),
            _ => Err(ValueConversionError::new(
                format!("Expected boolean, found {}.", value.to_type_string()).as_str(),
            )),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        if value { Value::True } else { Value::False }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}
