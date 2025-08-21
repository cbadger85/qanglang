mod arena;
#[allow(dead_code)]
mod hashmap_arena;
mod heap;
mod object;
mod string_interner;

pub use arena::{Arena, Index};
pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use heap::{
    ClassHandle, ClosureHandle, FunctionHandle, NativeFunctionHandle, ObjectHeap, UpvalueHandle,
};
pub use object::{ClosureObject, FunctionObject, Upvalue, UpvalueReference};
pub use string_interner::StringHandle;
