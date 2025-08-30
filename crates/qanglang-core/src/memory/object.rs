use crate::{
    ClassHandle, ClosureHandle, FunctionHandle, HashMapHandle, NativeFunctionError, Value, Vm,
    chunk::Chunk,
    memory::{
        StringHandle,
        closure_arena::{INLINE_UPVALUE_COUNT, OverflowHandle},
    },
};

// Re-export from closure_arena
pub use crate::memory::closure_arena::UpvalueSlot;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Upvalue {
    pub value: Value,
    pub is_marked: bool,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionHandle,
    pub upvalue_count: usize,
    pub inline_upvalues: [UpvalueSlot; INLINE_UPVALUE_COUNT],
    pub overflow_handle: Option<OverflowHandle>,
    pub is_marked: bool,
}

impl ClosureObject {
    pub fn new(function: FunctionHandle, upvalue_count: usize) -> Self {
        Self {
            function,
            upvalue_count,
            inline_upvalues: [UpvalueSlot::default(); INLINE_UPVALUE_COUNT],
            overflow_handle: None,
            is_marked: false,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ClassObject {
    pub name: StringHandle,
    pub super_clazz: Option<ClassHandle>,
    pub method_table: HashMapHandle,
    pub value_table: HashMapHandle,
    pub is_marked: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct InstanceObject {
    pub clazz: ClassHandle,
    pub table: HashMapHandle,
    pub is_marked: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct BoundMethodObject {
    pub receiver: Value,
    pub closure: ClosureHandle,
    pub is_marked: bool,
}

impl BoundMethodObject {
    pub fn new(receiver: Value, closure: ClosureHandle) -> Self {
        Self {
            receiver,
            closure,
            is_marked: false,
        }
    }
}

pub type NativeFn = fn(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError>;

#[derive(Debug, Clone)]
pub struct NativeFunctionObject {
    pub function: NativeFn,
    pub arity: usize,
    pub name_handle: StringHandle,
}

pub type IntrinsicFn =
    fn(receiver: Value, args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError>;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum IntrinsicKind {
    String(StringHandle),
    Number(StringHandle),
    Array(StringHandle),
    Function(StringHandle),
}

impl IntrinsicKind {
    pub fn string_handle(&self) -> StringHandle {
        match self {
            Self::String(handle) => *handle,
            Self::Number(handle) => *handle,
            Self::Array(handle) => *handle,
            Self::Function(handle) => *handle,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IntrinsicMethod {
    Native { function: IntrinsicFn, arity: usize },
    Call,
    Apply,
    NilSafeCall,
    NilSafeApply,
}

#[derive(Debug, Clone)]
pub struct BoundIntrinsicObject {
    pub receiver: Value,
    pub method: IntrinsicMethod,
    pub name_handle: StringHandle,
    pub is_marked: bool,
}

impl BoundIntrinsicObject {
    pub fn new(receiver: Value, method: IntrinsicMethod, name_handle: StringHandle) -> Self {
        Self {
            receiver,
            method,
            name_handle,
            is_marked: false,
        }
    }
}
