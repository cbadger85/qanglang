mod alloc;
mod arena;
#[allow(dead_code)]
mod hashmap_arena;
mod object;
mod string_interner;

pub use alloc::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, ClosureHandle, FunctionHandle,
    HeapAllocator, InstanceHandle, NativeFunctionHandle, UpvalueHandle,
};
pub use arena::{Arena, Index};
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, IntrinsicKind, IntrinsicMethod, NativeFn, NativeFunctionObject,
    Upvalue, UpvalueReference,
};
pub use string_interner::StringHandle;
