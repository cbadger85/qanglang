mod arena;
mod backend;
mod debug;
mod error;
mod frontend;
pub mod memory;

pub use backend::assembler::{Assembler, CompilerPipeline, QangProgram};
pub use backend::chunk::SourceLocation;
pub use backend::compiler::compile;
pub use backend::value::Value;
pub use backend::vm::Vm;
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{
    CompilerError, ErrorMessageFormat, ErrorReporter, NativeFunctionError, QangCompilerError,
    QangRuntimeError, Trace,
};
pub use frontend::source::SourceMap;
pub use frontend::tokenizer::{Token, TokenType, Tokenizer};
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

#[cfg(test)]
pub mod tests;
