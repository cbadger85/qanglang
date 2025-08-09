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

pub const fn get_value_type(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Function(_) => "function",
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionValueKind {
    QangFunction(ObjectHandle),
    NativeFunction(NativeFunction),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(ObjectHandle),
    Function(FunctionValueKind),
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
            Value::Function(function) => match function {
                FunctionValueKind::NativeFunction(function) => {
                    heap.get(function.name).and_then(|obj| match obj {
                        HeapObject::String(str) => Some(str.to_string()),
                        _ => None,
                    })
                }
                FunctionValueKind::QangFunction(handle) => match heap.get(*handle) {
                    Some(HeapObject::Function(function)) => {
                        heap.get(function.name).and_then(|obj| match obj {
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

    pub fn into_string(self, heap: &ObjectHeap) -> Result<Box<str>, ValueConversionError> {
        match self {
            Value::String(handle) => heap
                .get(handle)
                .cloned()
                .ok_or(ValueConversionError::new("Expected string, found nil."))
                .and_then(|v| v.try_into()),
            _ => Err(ValueConversionError::new(
                format!("Expected string, found {}.", get_value_type(&self)).as_str(),
            )),
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
                format!("Expected number, found {}.", get_value_type(&value)).as_str(),
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
                    get_value_type(&value)
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
                format!("Expected boolean, found {}.", get_value_type(&value)).as_str(),
            )),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}
