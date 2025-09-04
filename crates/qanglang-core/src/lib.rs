mod arena;
pub mod ast;
mod backend;
mod chunk;
mod compiler;
mod debug;
mod error;
mod frontend;
pub mod memory;
mod parser;
mod qang_std;
mod source;
mod tokenizer;
mod value;
mod vm;

pub use chunk::SourceLocation;
pub use compiler::{
    CompilerError, CompilerPipeline, CompilerVisitor, ErrorMessageFormat, QangProgram,
};
pub use debug::{disassemble_chunk, disassemble_program};
pub use error::{ErrorReporter, NativeFunctionError, QangRuntimeError, QangSyntaxError, Trace};
pub use memory::{
    BoundIntrinsicHandle, BoundIntrinsicObject, BoundMethodHandle, BoundMethodObject, ClassHandle,
    ClassObject, ClosureHandle, ClosureObject, FunctionHandle, FunctionObject, HashMapHandle,
    HeapAllocator, InstanceHandle, InstanceObject, IntrinsicFn, NativeFn, NativeFunctionHandle,
    NativeFunctionObject, StringHandle, Upvalue, UpvalueHandle, UpvalueSlot,
};
pub use parser::Parser;
pub use source::SourceMap;
pub use tokenizer::{Token, TokenType, Tokenizer};
pub use value::Value;
pub use vm::Vm;

#[cfg(test)]
pub mod tests;
