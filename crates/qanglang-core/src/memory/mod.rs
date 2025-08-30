mod alloc;
mod arena;
mod array_arena;
pub mod closure_upvalue_reference;
mod hashmap_arena;
mod object;
mod string_interner;
mod upvalue_overflow_arena;

pub use alloc::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, ClosureHandle, FunctionHandle,
    HeapAllocator, InstanceHandle, NativeFunctionHandle, UpvalueHandle,
};
pub use closure_upvalue_reference::ClosureUpvalueReference;
pub use upvalue_overflow_arena::UpvalueOverflowHandle;
pub use arena::{Arena, Index};
pub use array_arena::ArrayHandle;
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, IntrinsicKind, IntrinsicMethod, NativeFn, NativeFunctionObject,
    Upvalue, UpvalueReference,
};
pub use string_interner::StringHandle;
