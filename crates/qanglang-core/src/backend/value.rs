use crate::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, HashMapHandle, HeapAllocator,
    InstanceHandle, NativeFunctionHandle,
    arena::Index,
    memory::{ArrayHandle, ClosureHandle, FunctionHandle, StringHandle},
};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

// keywords
pub const NIL_TYPE_STRING: &str = "nil";
pub const BOOLEAN_TYPE_STRING: &str = "boolean";
pub const NUMBER_TYPE_STRING: &str = "number";
pub const STRING_TYPE_STRING: &str = "string";
pub const FUNCTION_TYPE_STRING: &str = "function";
pub const CLASS_TYPE_STRING: &str = "class";
pub const OBJECT_TYPE_STRING: &str = "object";
pub const ARRAY_TYPE_STRING: &str = "array";
pub const MODULE_TYPE_STRING: &str = "module";

const NAN_BASE: u64 = 0x7FF8_0000_0000_0000;
const TYPE_MASK: u64 = 0x0000_F000_0000_0000; // 4 bits for type
const PAYLOAD_MASK: u64 = 0x0000_0FFF_FFFF_FFFF; // 44 bits for data

// Type tags (in the high nibble of mantissa)
const NIL_TAG: u64 = 0x0000_1000_0000_0000;
const BOOL_TAG: u64 = 0x0000_2000_0000_0000; // +1 for false, +2 for true
const STRING_TAG: u64 = 0x0000_3000_0000_0000;
const CLOSURE_TAG: u64 = 0x0000_4000_0000_0000;
const NATIVE_TAG: u64 = 0x0000_5000_0000_0000;
const FUNC_TAG: u64 = 0x0000_6000_0000_0000;
const CLASS_TAG: u64 = 0x0000_7000_0000_0000;
const INSTANCE_TAG: u64 = 0x0000_8000_0000_0000;
const BOUND_METHOD_TAG: u64 = 0x0000_9000_0000_0000;
const BOUND_INTRINSIC_TAG: u64 = 0x0000_A000_0000_0000;
const ARRAY_TAG: u64 = 0x0000_B000_0000_0000;
const OBJECT_TAG: u64 = 0x0000_C000_0000_0000;
const MODULE_TAG: u64 = 0x0000_D000_0000_0000;
// Tags E and F still available

// Usage
const NIL_NAN: u64 = NAN_BASE | NIL_TAG | 1;
const TRUE_NAN: u64 = NAN_BASE | BOOL_TAG | 2;
const FALSE_NAN: u64 = NAN_BASE | BOOL_TAG | 1;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueKind {
    Nil,
    True,
    False,
    Number(f64),
    String(u32),
    Closure(Index),
    NativeFunction(u32),
    FunctionDecl(u32),
    Class(Index),
    Instance(Index),
    BoundMethod(Index),
    BoundIntrinsic(Index),
    Array(Index),
    ObjectLiteral(Index),
    Module(Index),
}

#[derive(Debug, Clone, Copy)]
pub struct Value(f64);

impl Value {
    pub fn nil() -> Self {
        Self(f64::from_bits(NIL_NAN))
    }

    pub fn is_nil(&self) -> bool {
        self.0.to_bits() == NIL_NAN
    }

    pub fn boolean(value: bool) -> Self {
        Self(f64::from_bits(if value { TRUE_NAN } else { FALSE_NAN }))
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self.0.to_bits() {
            TRUE_NAN => Some(true),
            FALSE_NAN => Some(false),
            _ => None,
        }
    }

    pub fn number(value: f64) -> Self {
        Self(value)
    }

    pub fn as_number(&self) -> Option<f64> {
        if !self.0.is_nan() { Some(self.0) } else { None }
    }

    pub fn string(handle: StringHandle) -> Self {
        let bits = NAN_BASE | STRING_TAG | (handle as u64 & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_string(&self) -> Option<StringHandle> {
        let bits = self.0.to_bits();
        if bits & TYPE_MASK == STRING_TAG {
            Some((bits & PAYLOAD_MASK) as u32)
        } else {
            None
        }
    }

    pub fn function(handle: FunctionHandle) -> Self {
        let bits = NAN_BASE | FUNC_TAG | (handle as u64 & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_function(&self) -> Option<FunctionHandle> {
        let bits = self.0.to_bits();
        if bits & TYPE_MASK == FUNC_TAG {
            Some((bits & PAYLOAD_MASK) as u32)
        } else {
            None
        }
    }

    pub fn native_function(handle: NativeFunctionHandle) -> Self {
        let bits = NAN_BASE | NATIVE_TAG | (handle as u64 & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_native_function(&self) -> Option<NativeFunctionHandle> {
        let bits = self.0.to_bits();
        if bits & TYPE_MASK == NATIVE_TAG {
            Some((bits & PAYLOAD_MASK) as u32)
        } else {
            None
        }
    }

    pub fn closure(handle: ClosureHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | CLOSURE_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_closure(&self) -> Option<ClosureHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == CLOSURE_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn class(handle: ClassHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | CLASS_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_class(&self) -> Option<ClassHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == CLASS_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn instance(handle: InstanceHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | INSTANCE_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_instance(&self) -> Option<InstanceHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == INSTANCE_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn bound_method(handle: BoundMethodHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | BOUND_METHOD_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_bound_method(&self) -> Option<BoundMethodHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == BOUND_METHOD_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn bound_intrinsic(handle: BoundIntrinsicHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | BOUND_INTRINSIC_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_bound_intrinsic(&self) -> Option<BoundIntrinsicHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == BOUND_INTRINSIC_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn array(handle: ArrayHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | ARRAY_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_array(&self) -> Option<ArrayHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == ARRAY_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn object_literal(handle: HashMapHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | OBJECT_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_object_literal(&self) -> Option<HashMapHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == OBJECT_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn module(handle: HashMapHandle) -> Self {
        let packed = handle.pack_for_nan();
        let bits = NAN_BASE | MODULE_TAG | (packed & PAYLOAD_MASK);
        Self(f64::from_bits(bits))
    }

    pub fn as_module(&self) -> Option<HashMapHandle> {
        let bits = self.0.to_bits();
        if (bits & TYPE_MASK) == MODULE_TAG {
            let packed = bits & PAYLOAD_MASK;
            Some(Index::unpack_from_nan(packed))
        } else {
            None
        }
    }

    pub fn kind(self) -> ValueKind {
        if !self.0.is_nan() {
            return ValueKind::Number(self.0);
        }

        let bits = self.0.to_bits();
        match bits & TYPE_MASK {
            NIL_TAG => ValueKind::Nil,
            BOOL_TAG => {
                let payload = bits & PAYLOAD_MASK;
                if payload == 2 {
                    ValueKind::True
                } else {
                    ValueKind::False
                }
            }
            STRING_TAG => {
                let handle = (bits & PAYLOAD_MASK) as u32;
                ValueKind::String(handle)
            }
            FUNC_TAG => {
                let handle = (bits & PAYLOAD_MASK) as u32;
                ValueKind::FunctionDecl(handle)
            }
            NATIVE_TAG => {
                let handle = (bits & PAYLOAD_MASK) as u32;
                ValueKind::NativeFunction(handle)
            }
            CLOSURE_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::Closure(index)
            }
            CLASS_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::Class(index)
            }
            INSTANCE_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::Instance(index)
            }
            BOUND_METHOD_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::BoundMethod(index)
            }
            BOUND_INTRINSIC_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::BoundIntrinsic(index)
            }
            ARRAY_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::Array(index)
            }
            OBJECT_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::ObjectLiteral(index)
            }
            MODULE_TAG => {
                let packed = bits & PAYLOAD_MASK;
                let index = Index::unpack_from_nan(packed);
                ValueKind::Module(index)
            }
            _ => unreachable!("Invalid NaN pattern"),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self.as_boolean() {
            Some(b) => b,
            None => !self.is_nil(), // Everything except nil and false is truthy
        }
    }

    pub fn print(&self, allocator: &HeapAllocator) {
        print!("{}", self.to_display_string(allocator));
    }

    pub fn to_display_string(&self, allocator: &HeapAllocator) -> String {
        match self.kind() {
            ValueKind::Nil => "nil".to_string(),
            ValueKind::Number(number) => number.to_string(),
            ValueKind::String(handle) => allocator.strings.get_string(handle).to_string(),
            ValueKind::True => "true".to_string(),
            ValueKind::False => "false".to_string(),
            ValueKind::FunctionDecl(function_handle) => {
                let function = allocator.get_function(function_handle);
                let identifier = allocator.strings.get_string(function.name);
                format!("{}<function>", identifier)
            }
            ValueKind::Closure(handle) => {
                let closure = allocator.closures.get_closure(handle);
                let function = allocator.get_function(closure.function);
                let identifier = allocator.strings.get_string(function.name);
                format!("{}<function>", identifier)
            }
            ValueKind::NativeFunction(handle) => {
                let function = allocator.get_native_function(handle);
                let identifier = allocator.strings.get_string(function.name_handle);
                format!("{}<function>", identifier)
            }
            ValueKind::Class(handle) => {
                let clazz = allocator.get_class(handle);
                let identifier = allocator.strings.get_string(clazz.name);
                format!("{}<class>", identifier)
            }
            ValueKind::Instance(handle) => {
                let instance = allocator.get_instance(handle);
                let clazz = allocator.get_class(instance.clazz);
                let identifier = allocator.strings.get_string(clazz.name);
                format!("instanceof {}", identifier)
            }
            ValueKind::BoundMethod(handle) => {
                let method_binding = allocator.get_bound_method(handle);
                format!(
                    "{}.{}",
                    method_binding.receiver.to_display_string(allocator),
                    Value::closure(method_binding.closure).to_display_string(allocator)
                )
            }
            ValueKind::BoundIntrinsic(handle) => {
                let intrinsic_binding = allocator.get_bound_intrinsic(handle);
                format!(
                    "{}.{}",
                    intrinsic_binding.receiver.to_type_string(),
                    Value::string(intrinsic_binding.name_handle).to_display_string(allocator)
                )
            }
            ValueKind::Array(handle) => {
                let mut string = String::from("[");
                for item in allocator.arrays.iter(handle) {
                    string.push_str(&item.to_display_string(allocator));
                    string.push(',');
                }
                string.push(']');
                string
            }
            ValueKind::ObjectLiteral(handle) => {
                let mut string = String::from("{{");
                for (key, value) in allocator.tables.iter(handle) {
                    string.push(' ');
                    string.push_str(&key.to_display_string(allocator));
                    string.push('=');
                    string.push_str(&value.to_display_string(allocator));
                    string.push(',');
                    string.push(' ');
                }
                string.push_str("}}");
                string
            }
            ValueKind::Module(handle) => {
                format!("module#{}", handle)
            }
        }
    }

    pub fn to_type_string(&self) -> &'static str {
        match self.kind() {
            ValueKind::Nil => NIL_TYPE_STRING,
            ValueKind::True => BOOLEAN_TYPE_STRING,
            ValueKind::False => BOOLEAN_TYPE_STRING,
            ValueKind::Number(_) => NUMBER_TYPE_STRING,
            ValueKind::String(_) => STRING_TYPE_STRING,
            ValueKind::Closure(_) => FUNCTION_TYPE_STRING,
            ValueKind::NativeFunction(_) => FUNCTION_TYPE_STRING,
            ValueKind::FunctionDecl(_) => FUNCTION_TYPE_STRING,
            ValueKind::Class(_) => CLASS_TYPE_STRING,
            ValueKind::Instance(_) => OBJECT_TYPE_STRING,
            ValueKind::BoundMethod(_) => FUNCTION_TYPE_STRING,
            ValueKind::BoundIntrinsic(_) => FUNCTION_TYPE_STRING,
            ValueKind::Array(_) => ARRAY_TYPE_STRING,
            ValueKind::ObjectLiteral(_) => OBJECT_TYPE_STRING,
            ValueKind::Module(_) => MODULE_TYPE_STRING,
        }
    }

    pub fn hash(&self) -> u64 {
        let mut hasher = FxHasher::default();

        self.0.to_bits().hash(&mut hasher);

        hasher.finish()
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::number(num)
    }
}

impl From<u64> for Value {
    fn from(num: u64) -> Self {
        Value::number(num as f64)
    }
}

impl From<usize> for Value {
    fn from(num: usize) -> Self {
        Value::number(num as f64)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::boolean(value)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // For regular numbers (not NaN), use direct comparison
        if !self.0.is_nan() && !other.0.is_nan() {
            return self.0 == other.0;
        }

        // For NaN patterns, compare the bit representation directly
        // This ensures different NaN values (nil, booleans, strings, etc.) compare correctly
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::nil()
    }
}

impl std::fmt::Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.index)
    }
}
