mod arena;
mod backend;
mod debug;
mod error;
mod frontend;
pub mod memory;

pub use backend::assembler::QangProgram;
pub use backend::chunk::SourceLocation;
pub use backend::compiler::{CompilerConfig, CompilerPipeline};
pub use backend::value::Value;
pub use backend::vm::Vm;
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{
    ErrorMessageFormat, ErrorReporter, NativeFunctionError, QangCompilerError, QangPipelineError,
    QangRuntimeError, Trace,
};
pub use frontend::source::SourceMap;
pub use frontend::tokenizer::{Token, TokenType, Tokenizer};
pub use frontend::{
    analyzer::{AnalysisPipeline, AnalysisPipelineConfig, AnalysisResults},
    node_array_arena::NodeArrayId,
    nodes,
    parse::Parser,
    typed_node_arena::{NodeId, TypedNodeArena},
};
pub use memory::{
    BoundIntrinsicHandle, BoundIntrinsicObject, BoundMethodHandle, BoundMethodObject, ClassHandle,
    ClassObject, ClosureHandle, ClosureObject, FunctionHandle, FunctionObject, HashMapHandle,
    HeapAllocator, InstanceHandle, InstanceObject, IntrinsicFn, NativeFn, NativeFunctionHandle,
    NativeFunctionObject, StringHandle, StringInterner, Upvalue, UpvalueHandle, UpvalueSlot,
};

#[cfg(test)]
pub mod tests;
