use crate::{HeapObject, NativeFn, ObjectHeap, error::ValueConversionError, heap::ObjectHandle};

#[derive(Debug, Clone, Copy)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub arity: usize,
    pub name: ObjectHandle,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionValueKind {
    Closure(ObjectHandle),
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
    String(ObjectHandle),
    Function(FunctionValueKind),
    FunctionDecl(ObjectHandle),
}

impl Value {
    pub fn print(&self, heap: &ObjectHeap) {
        print!("{}", self.to_display_string(heap));
    }

    pub fn to_display_string(&self, heap: &ObjectHeap) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(number) => number.to_string(),
            Value::String(handle) => {
                if let Some(HeapObject::String(str)) = heap.get(*handle) {
                    return format!("{}", str);
                }
                "nil".to_string()
            }
            Value::Boolean(boolean) => boolean.to_string(),
            Value::FunctionDecl(_) => "nil".to_string(),
            Value::Function(function) => match function {
                FunctionValueKind::NativeFunction(function) => {
                    heap.get(function.name).and_then(|obj| match obj {
                        HeapObject::String(str) => Some(str.to_string()),
                        _ => None,
                    })
                }
                FunctionValueKind::Closure(handle) => match heap.get(*handle) {
                    Some(HeapObject::Closure(closure)) => {
                        heap.get(closure.function.name).and_then(|obj| match obj {
                            HeapObject::String(str) => Some(str.to_string()),
                            _ => None,
                        })
                    }
                    _ => None,
                },
            }
            .unwrap_or("nil".to_string()),
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

    pub fn into_string(self, heap: &ObjectHeap) -> Result<Box<str>, ValueConversionError> {
        match self {
            Value::String(handle) => heap
                .get(handle)
                .cloned()
                .ok_or(ValueConversionError::new("Expected string, found nil."))
                .and_then(|v| v.try_into()),
            _ => Err(ValueConversionError::new(
                format!("Expected string, found {}.", self.to_type_string()).as_str(),
            )),
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

impl TryFrom<Value> for ObjectHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(handle) => Ok(handle),
            _ => Err(ValueConversionError::new(
                format!(
                    "Expected referenced value, found {}.",
                    value.to_type_string()
                )
                .as_str(),
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
