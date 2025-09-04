mod arena;
mod backend;
mod chunk;
mod debug;
mod error;
mod frontend;
pub mod memory;
mod qang_std;
mod source;
mod tokenizer;
mod value;
mod vm;

pub use backend::assembler::{Assembler, CompilerPipeline, QangProgram};
pub use chunk::SourceLocation;
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{
    CompilerError, ErrorMessageFormat, ErrorReporter, NativeFunctionError, QangRuntimeError,
    QangSyntaxError, Trace,
};
pub use frontend::{
    node_array_arena::NodeArrayId,
    nodes,
    parse::Parser,
    typed_node_arena::{NodeId, TypedNodeArena},
};
pub use memory::{
    BoundIntrinsicHandle, BoundIntrinsicObject, BoundMethodHandle, BoundMethodObject, ClassHandle,
    ClassObject, ClosureHandle, ClosureObject, FunctionHandle, FunctionObject, HashMapHandle,
    HeapAllocator, InstanceHandle, InstanceObject, IntrinsicFn, NativeFn, NativeFunctionHandle,
    NativeFunctionObject, StringHandle, Upvalue, UpvalueHandle, UpvalueSlot,
};
pub use source::SourceMap;
pub use tokenizer::{Token, TokenType, Tokenizer};
pub use value::Value;
pub use vm::Vm;

#[cfg(test)]
pub mod tests;
