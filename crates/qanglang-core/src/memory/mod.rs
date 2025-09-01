mod alloc;
mod array_arena;
mod closure_arena;
pub mod closure_upvalue_reference;
mod hashmap_arena;
mod object;
mod string_interner;
mod upvalue_overflow_arena;

pub use alloc::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, FunctionHandle, HeapAllocator,
    InstanceHandle, NativeFunctionHandle, UpvalueHandle,
};
pub use array_arena::ArrayHandle;
pub use closure_arena::{ClosureArena, ClosureHandle};
pub use closure_upvalue_reference::OpenUpvalueTracker;
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, IntrinsicKind, IntrinsicMethod, NativeFn, NativeFunctionObject,
    Upvalue, UpvalueSlot,
};
pub use string_interner::StringHandle;
pub use upvalue_overflow_arena::UpvalueOverflowArena;
