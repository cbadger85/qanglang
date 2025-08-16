use crate::{
    NativeFn, ObjectHeap,
    error::ValueConversionError,
    memory::{ClosureHandle, FunctionHandle, StringHandle},
};

#[derive(Debug, Clone, Copy)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub arity: usize,
    pub name: StringHandle,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Boolean(bool),
    Number(f64),
    String(StringHandle),
    Function(FunctionValueKind),
    FunctionDecl(FunctionHandle),
}

impl Value {
    pub fn print(&self, heap: &ObjectHeap) {
        print!("{}", self.to_display_string(heap));
    }

    // TODO change this to return a string slice.
    pub fn to_display_string(&self, heap: &ObjectHeap) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(number) => number.to_string(),
            Value::String(handle) => heap.get_string(*handle).to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::FunctionDecl(function_handle) => {
                let handle = heap.get_function(*function_handle).name;
                let identifier = heap.get_string(handle);

                format!("<function>{}", identifier)
            }
            Value::Function(function) => match function {
                FunctionValueKind::NativeFunction(function) => {
                    let identifier = heap.get_string(function.name);
                    format!("<function>{}", identifier)
                }
                FunctionValueKind::Closure(handle) => {
                    let handle = heap.get_closure(*handle).function.name;
                    let identifier = heap.get_string(handle);
                    format!("<function>{}", identifier)
                }
            },
        }
    }

    pub const fn to_type_string(&self) -> &'static str {
        match self {
            Value::Nil => NIL_TYPE_STRING,
            Value::Boolean(_) => BOOLEAN_TYPE_STRING,
            Value::Number(_) => NUMBER_TYPE_STRING,
            Value::String(_) => STRING_TYPE_STRING,
            Value::Function(_) => FUNCTION_TYPE_STRING,
            Value::FunctionDecl(_) => FUNCTION_TYPE_STRING,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(boolean) => *boolean,
            Value::Nil => false,
            _ => true,
        }
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::Number(num)
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
            Value::Boolean(boolean) => Ok(boolean),
            _ => Err(ValueConversionError::new(
                format!("Expected boolean, found {}.", value.to_type_string()).as_str(),
            )),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}
