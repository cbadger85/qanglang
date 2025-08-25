use crate::{
    BoundMethodHandle, ClassHandle, HeapAllocator, InstanceHandle, NativeFunctionHandle,
    memory::{ArrayHandle, ClosureHandle, FunctionHandle, StringHandle},
};

// keywords
pub const NIL_TYPE_STRING: &str = "nil";
pub const BOOLEAN_TYPE_STRING: &str = "boolean";
pub const NUMBER_TYPE_STRING: &str = "number";
pub const STRING_TYPE_STRING: &str = "string";
pub const FUNCTION_TYPE_STRING: &str = "function";
pub const CLASS_TYPE_STRING: &str = "class";
pub const OBJECT_TYPE_STRING: &str = "object";
pub const ARRAY_TYPE_STRING: &str = "array";
pub const CLASS_INITIALIZER_STRING: &str = "init";

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
    Instance(InstanceHandle),
    BoundMethod(BoundMethodHandle),
    BoundIntrinsic(BoundMethodHandle),
    Array(ArrayHandle),
}

impl Value {
    pub fn print(&self, allocator: &HeapAllocator) {
        print!("{}", self.to_display_string(allocator));
    }

    pub fn to_display_string(&self, allocator: &HeapAllocator) -> String {
        // TODO delete this function later when the `is` operator is implemented.
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(number) => number.to_string(),
            Value::String(handle) => allocator.strings.get_string(*handle).to_string(),
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::FunctionDecl(function_handle) => {
                let function = allocator.get_function(*function_handle);
                let identifier = allocator.strings.get_string(function.name);
                format!("{}<function>", identifier)
            }
            Value::Closure(handle) => {
                let closure = allocator.get_closure(*handle);
                let function = allocator.get_function(closure.function);
                let identifier = allocator.strings.get_string(function.name);
                format!("{}<function>", identifier)
            }
            Value::NativeFunction(handle) => {
                let function = allocator.get_native_function(*handle);
                let identifier = allocator.strings.get_string(function.name_handle);
                format!("{}<function>", identifier)
            }
            Value::Class(handle) => {
                let clazz = allocator.get_class(*handle);
                let identifier = allocator.strings.get_string(clazz.name);
                format!("{}<class>", identifier)
            }
            Value::Instance(handle) => {
                let instance = allocator.get_instance(*handle);
                let clazz = allocator.get_class(instance.clazz);
                let identifier = allocator.strings.get_string(clazz.name);
                format!("instanceof {}", identifier)
            }
            Value::BoundMethod(handle) => {
                let method_binding = allocator.get_bound_method(*handle);
                format!(
                    "{}.{}",
                    method_binding.receiver.to_display_string(allocator),
                    Value::Closure(method_binding.closure).to_display_string(allocator)
                )
            }
            Value::BoundIntrinsic(handle) => {
                let intrinsic_binding = allocator.get_bound_intrinsic(*handle);
                format!(
                    "{}.{}",
                    intrinsic_binding.receiver.to_type_string(),
                    Value::String(intrinsic_binding.name_handle).to_display_string(allocator)
                )
            }
            Value::Array(handle) => {
                let mut string = String::from("[");
                for item in allocator.arrays.iter(*handle) {
                    string.push_str(&item.to_display_string(allocator));
                    string.push(',');
                }
                string.push(']');
                string
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
            Value::Instance(_) => OBJECT_TYPE_STRING,
            Value::BoundMethod(_) => FUNCTION_TYPE_STRING,
            Value::BoundIntrinsic(_) => FUNCTION_TYPE_STRING,
            Value::Array(_) => ARRAY_TYPE_STRING,
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::False)
    }

    pub fn hash(&self) -> u64 {
        use rustc_hash::FxHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = FxHasher::default();
        std::mem::discriminant(self).hash(&mut hasher);
        match self {
            Value::Number(n) => n.to_bits().hash(&mut hasher),
            Value::String(handle) => handle.hash(&mut hasher),
            Value::Closure(handle) => handle.hash(&mut hasher),
            Value::NativeFunction(handle) => handle.hash(&mut hasher),
            Value::FunctionDecl(handle) => handle.hash(&mut hasher),
            Value::Class(handle) => handle.hash(&mut hasher),
            Value::Instance(handle) => handle.hash(&mut hasher),
            Value::BoundMethod(handle) => handle.hash(&mut hasher),
            Value::BoundIntrinsic(handle) => handle.hash(&mut hasher),
            Value::Array(handle) => handle.hash(&mut hasher),
            Value::Nil | Value::True | Value::False => (),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::Index;

    #[test]
    fn test_hash_different_variants_have_different_hashes() {
        let string_value = Value::String(42);
        let function_value = Value::FunctionDecl(42);
        let closure_value = Value::Closure(Index::new(42, 0));
        let class_value = Value::Class(Index::new(42, 0));
        let native_fn_value = Value::NativeFunction(42);

        assert_ne!(string_value.hash(), function_value.hash());
        assert_ne!(string_value.hash(), closure_value.hash());
        assert_ne!(string_value.hash(), class_value.hash());
        assert_ne!(string_value.hash(), native_fn_value.hash());
        assert_ne!(function_value.hash(), closure_value.hash());
        assert_ne!(function_value.hash(), class_value.hash());
        assert_ne!(closure_value.hash(), class_value.hash());
    }

    #[test]
    fn test_hash_same_values_have_same_hashes() {
        assert_eq!(Value::Nil.hash(), Value::Nil.hash());
        assert_eq!(Value::True.hash(), Value::True.hash());
        assert_eq!(Value::False.hash(), Value::False.hash());
        assert_eq!(Value::Number(42.0).hash(), Value::Number(42.0).hash());
        assert_eq!(Value::String(123).hash(), Value::String(123).hash());
    }
}
