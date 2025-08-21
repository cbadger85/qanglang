use crate::{
    ClassHandle, HeapAllocator, NativeFn, NativeFunctionHandle,
    error::ValueConversionError,
    memory::{ClosureHandle, FunctionHandle, StringHandle},
};

#[derive(Debug, Clone, Copy)]
pub struct NativeFunctionObject {
    pub function: NativeFn,
    pub arity: usize,
    pub name_handle: StringHandle,
}

pub const NIL_TYPE_STRING: &str = "nil";
pub const BOOLEAN_TYPE_STRING: &str = "boolean";
pub const NUMBER_TYPE_STRING: &str = "number";
pub const STRING_TYPE_STRING: &str = "string";
pub const FUNCTION_TYPE_STRING: &str = "function";
pub const CLASS_TYPE_STRING: &str = "class";

#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(StringHandle),
    Closure(ClosureHandle),
    NativeFunction(NativeFunctionHandle),
    FunctionDecl(FunctionHandle),
    Class(ClassHandle),
}

impl Value {
    pub fn print(&self, allocator: &HeapAllocator) {
        print!("{}", self.to_display_string(allocator));
    }

    pub fn to_display_string(&self, allocator: &HeapAllocator) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(number) => number.to_string(),
            Value::String(handle) => allocator.strings.get_string(*handle).to_string(),
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::FunctionDecl(function_handle) => {
                let function = allocator.get_function(*function_handle);
                let identifier = allocator.strings.get_string(function.name);
                format!("<function>{}", identifier)
            }
            Value::Closure(handle) => {
                let closure = allocator.get_closure(*handle);
                let function = allocator.get_function(closure.function);
                let identifier = allocator.strings.get_string(function.name);
                format!("<function>{}", identifier)
            }
            Value::NativeFunction(handle) => {
                let function = allocator.get_native_function(*handle);
                let identifier = allocator.strings.get_string(function.name_handle);
                format!("<function>{}", identifier)
            }
            Value::Class(handle) => {
                let clazz = allocator.get_class(*handle);
                let identifier = allocator.strings.get_string(clazz.name);
                format!("<class>{}", identifier)
            }
        }
    }

    pub const fn to_type_string(&self) -> &'static str {
        match self {
            Value::Nil => NIL_TYPE_STRING,
            Value::True => BOOLEAN_TYPE_STRING,
            Value::False => BOOLEAN_TYPE_STRING,
            Value::Number(_) => NUMBER_TYPE_STRING,
            Value::String(_) => STRING_TYPE_STRING,
            Value::Closure(_) => FUNCTION_TYPE_STRING,
            Value::NativeFunction(_) => FUNCTION_TYPE_STRING,
            Value::FunctionDecl(_) => FUNCTION_TYPE_STRING,
            Value::Class(_) => CLASS_TYPE_STRING,
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::False)
    }

    pub fn hash(&self) -> u64 {
        use rustc_hash::FxHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = FxHasher::default();
        match self {
            Value::Nil => 0u8.hash(&mut hasher),
            Value::True => true.hash(&mut hasher),
            Value::False => false.hash(&mut hasher),
            Value::Number(n) => n.to_bits().hash(&mut hasher),
            Value::String(handle) => handle.hash(&mut hasher),
            Value::Closure(handle) => handle.hash(&mut hasher),
            Value::NativeFunction(func) => func.hash(&mut hasher),
            Value::FunctionDecl(handle) => handle.hash(&mut hasher),
            Value::Class(handle) => handle.hash(&mut hasher),
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
