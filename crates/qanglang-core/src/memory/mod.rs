mod hashmap_arena;
mod heap;
mod object;

pub use hashmap_arena::{BucketChunkHandle, HashMapHandle};
pub use heap::{ClosureHandle, FunctionHandle, ObjectHeap, StringHandle, UpvalueHandle};
pub use object::{ClosureObject, FunctionObject, HashMapObject, Upvalue, UpvalueReference};
