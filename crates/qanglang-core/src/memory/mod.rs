mod alloc;
mod arena;
mod array_arena;
mod closure_arena;
pub mod closure_upvalue_reference;
mod hashmap_arena;
mod object;
mod string_interner;

pub use alloc::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, FunctionHandle,
    HeapAllocator, InstanceHandle, NativeFunctionHandle, UpvalueHandle,
};
pub use arena::{Arena, Index};
pub use array_arena::ArrayHandle;
pub use closure_arena::{ClosureArena, ClosureHandle};
pub use closure_upvalue_reference::ClosureUpvalueReference;
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, IntrinsicKind, IntrinsicMethod, NativeFn, NativeFunctionObject,
    Upvalue, UpvalueReference,
};
pub use string_interner::StringHandle;
