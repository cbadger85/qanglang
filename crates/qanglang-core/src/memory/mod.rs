mod alloc;
mod arena;
#[allow(dead_code)]
mod hashmap_arena;
mod object;
mod string_interner;

pub use alloc::{
    ClassHandle, ClosureHandle, FunctionHandle, HeapAllocator, NativeFunctionHandle, UpvalueHandle,
};
pub use arena::{Arena, Index};
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use object::{ClassObject, ClosureObject, FunctionObject, Upvalue, UpvalueReference};
pub use string_interner::StringHandle;
