mod arena;
mod backend;
mod debug;
mod error;
mod frontend;
pub mod memory;
pub mod pipeline;

pub use backend::assembler::QangProgram;
pub use backend::chunk::SourceLocation;
pub use backend::module_resolver::{ModuleResolver, RuntimeModule};
pub use backend::object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, NativeFn, NativeFunctionObject, Upvalue,
};
pub use backend::value::{Value, ValueKind};
pub use backend::vm::Vm;
pub use backend::STDLIB_SOURCE;
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{
    ErrorMessageFormat, ErrorReporter, NativeFunctionError, QangCompilerError, QangPipelineError,
    QangRuntimeError, Trace,
};
pub use frontend::module_map::ModuleMap;
pub use frontend::source::SourceMap;
pub use frontend::tokenizer::{Token, TokenType, Tokenizer};
pub use frontend::{
    analyzer::{AnalysisPipeline, AnalysisPipelineConfig},
    ast_node_arena::{AstNodeArena, NodeId, TypedNodeRef},
    node_array_arena::NodeArrayId,
    nodes,
    parser::{Parser, ParserConfig},
    symbol_resolver,
};
pub use memory::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, ClosureHandle, FunctionHandle,
    HashMapHandle, HeapAllocator, InstanceHandle, NativeFunctionHandle, StringHandle,
    StringInterner, UpvalueHandle, UpvalueSlot,
};
pub use pipeline::GlobalCompilerPipeline;

#[cfg(test)]
pub mod tests;
