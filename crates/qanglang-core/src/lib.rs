mod arena;
mod backend;
mod debug;
mod error;
mod frontend;
pub mod memory;

pub use backend::chunk::SourceLocation;
pub use backend::compiler::QangProgram;
pub use backend::compiler::{CompilerConfig, CompilerPipeline};
pub use backend::module_resolver::{ModuleResolver, RuntimeModule};
pub use backend::object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, ClosureObject, FunctionObject,
    InstanceObject, IntrinsicFn, NativeFn, NativeFunctionObject, Upvalue,
};
pub use backend::value::{Value, ValueKind};
pub use backend::vm::Vm;
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{
    ErrorMessageFormat, ErrorReporter, NativeFunctionError, QangCompilerError, QangPipelineError,
    QangRuntimeError, Trace,
};
pub use frontend::source::SourceMap;
pub use frontend::tokenizer::{Token, TokenType, Tokenizer};
pub use frontend::{
    analyzer::{AnalysisPipeline, AnalysisPipelineConfig},
    node_array_arena::NodeArrayId,
    nodes,
    parse::{Parser, ParserConfig},
    typed_node_arena::{NodeId, TypedNodeArena},
};
pub use memory::{
    BoundIntrinsicHandle, BoundMethodHandle, ClassHandle, ClosureHandle, FunctionHandle,
    HashMapHandle, HeapAllocator, InstanceHandle, NativeFunctionHandle, StringHandle,
    StringInterner, UpvalueHandle, UpvalueSlot,
};

#[cfg(test)]
pub mod tests;
