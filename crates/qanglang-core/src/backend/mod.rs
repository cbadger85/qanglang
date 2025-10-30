pub mod assembler;
pub mod builtin_modules;
pub mod chunk;
pub mod external_context;
pub mod module_resolver;
pub mod object;
pub mod qang_std;
pub mod value;
pub mod vm;

/// The source code of the standard library
pub const STDLIB_SOURCE: &str = include_str!("stdlib.ql");
