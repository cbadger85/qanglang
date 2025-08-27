mod alloc;
mod arena;
mod array_arena;
mod hashmap_arena;
mod object;
mod string_interner;

pub use alloc::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, ClosureHandle, FunctionHandle,
    HeapAllocator, InstanceHandle, NativeFunctionHandle, UpvalueHandle,
};
pub use arena::{Arena, Index};
pub use array_arena::ArrayHandle;
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, IntrinsicKind, IntrinsicMethod, IntrinsicMethodKind, NativeFn,
    NativeFunctionObject, Upvalue, UpvalueReference,
};
pub use string_interner::StringHandle;
