mod analyzer;
mod builtins;
mod completion;
#[cfg(test)]
mod completion_test;
mod hover_utils;
#[cfg(test)]
mod hover_utils_test;
mod semantic_tokens;
mod server;
mod stdlib_analyzer;
mod symbol_collector;

pub use crate::server::run_language_server;
