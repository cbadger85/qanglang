use crate::{
    HeapObject, ObjectHeap,
    error::ValueConversionError,
    heap::{FunctionObject, ObjectHandle},
};

pub const fn get_value_type(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Function(_) => "function",
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(ObjectHandle),
    Function(ObjectHandle),
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
                if let Some(object) = heap.get(*handle) {
                    if let HeapObject::String(str) = &object {
                        return format!("{}", str);
                    }
                }
                "nil".to_string()
            }
            Value::Boolean(boolean) => boolean.to_string(),
            Value::Function(handle) => {
                if let Some(object) = heap.get(*handle) {
                    match object {
                        HeapObject::Function(FunctionObject::KangFunction(function)) => {
                            let name_handle = function.name;

                            if let Some(HeapObject::String(string)) = heap.get(name_handle) {
                                string.clone().into_string()
                            } else {
                                "nil".to_string()
                            }
                        }
                        HeapObject::Function(FunctionObject::NativeFunction(function)) => {
                            let name_handle = function.name;

                            if let Some(HeapObject::String(string)) = heap.get(name_handle) {
                                string.clone().into_string()
                            } else {
                                "nil".to_string()
                            }
                        }
                        _ => "nil".to_string(),
                    }
                } else {
                    "nil".to_string()
                }
            }
        }
    }

    pub fn into_string(self, heap: &ObjectHeap) -> Result<Box<str>, ValueConversionError> {
        match self {
            Value::String(handle) => heap
                .get(handle)
                .map(|h| h.clone())
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
