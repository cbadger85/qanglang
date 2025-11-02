use once_cell::sync::Lazy;
use qanglang_core::nodes::*;
use qanglang_core::{AstNodeArena, Parser, ParserConfig, SourceMap, StringInterner};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Information about a stdlib symbol
#[derive(Debug, Clone)]
pub enum StdlibSymbol {
    Class { name: String },
    Function { name: String },
    Method { name: String },
}

/// Cached stdlib information
pub struct StdlibCache {
    symbols: HashMap<String, StdlibSymbol>,
}

impl StdlibCache {
    /// Get a symbol by name
    pub fn get(&self, name: &str) -> Option<&StdlibSymbol> {
        self.symbols.get(name)
    }

    /// Get all class names
    pub fn get_classes(&self) -> impl Iterator<Item = &String> {
        self.symbols.iter().filter_map(|(name, symbol)| {
            if matches!(symbol, StdlibSymbol::Class { .. }) {
                Some(name)
            } else {
                None
            }
        })
    }

    /// Get all function names
    pub fn get_functions(&self) -> impl Iterator<Item = &String> {
        self.symbols.iter().filter_map(|(name, symbol)| {
            if matches!(symbol, StdlibSymbol::Function { .. }) {
                Some(name)
            } else {
                None
            }
        })
    }
}

/// Parse the stdlib and extract symbol information
fn parse_stdlib() -> StdlibCache {
    let stdlib_source = qanglang_core::STDLIB_SOURCE;

    let mut nodes = AstNodeArena::new();
    let mut strings = StringInterner::new();
    let source_map = Arc::new(SourceMap::new(
        stdlib_source.to_owned(),
        PathBuf::from("stdlib.ql"),
    ));

    let mut parser = Parser::new(source_map, &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let modules = parser.parse();

    // Get the root module ID
    let root_id = match modules.get_main().map(|m| m.node) {
        Some(id) => id,
        None => {
            // If parsing fails, return empty cache
            return StdlibCache {
                symbols: HashMap::new(),
            };
        }
    };

    let mut symbols = HashMap::new();

    // Get the root module
    let module = nodes.get_program_node(root_id);
    let decls_length = nodes.array.size(module.node.decls);

    // Iterate through all top-level declarations
    for i in 0..decls_length {
        if let Some(decl_id) = nodes.array.get_node_id_at(module.node.decls, i) {
            let decl = nodes.get_decl_node(decl_id);

            match decl.node {
                DeclNode::Class(class_decl) => {
                    // Extract class name
                    let name_node = nodes.get_identifier_node(class_decl.name);
                    let class_name = strings.get(name_node.node.name).to_string();

                    symbols.insert(
                        class_name.clone(),
                        StdlibSymbol::Class {
                            name: class_name.clone(),
                        },
                    );

                    // Extract methods from this class
                    let members_length = nodes.array.size(class_decl.members);
                    for j in 0..members_length {
                        if let Some(member_id) = nodes.array.get_node_id_at(class_decl.members, j) {
                            let member = nodes.get_class_member_node(member_id);

                            if let ClassMemberNode::Method(method) = member.node {
                                let method_name_node = nodes.get_identifier_node(method.name);
                                let method_name =
                                    strings.get(method_name_node.node.name).to_string();

                                // Store method with a key that includes class name
                                let key = format!("{}.{}", class_name, method_name);
                                symbols.insert(
                                    key,
                                    StdlibSymbol::Method {
                                        name: method_name.clone(),
                                    },
                                );
                            }
                        }
                    }
                }
                DeclNode::Function(func_decl) => {
                    // Extract function name and parameter count
                    let func_expr = nodes.get_func_expr_node(func_decl.function);
                    let name_node = nodes.get_identifier_node(func_expr.node.name);
                    let func_name = strings.get(name_node.node.name).to_string();

                    symbols.insert(
                        func_name.clone(),
                        StdlibSymbol::Function { name: func_name },
                    );
                }
                _ => {}
            }
        }
    }

    StdlibCache { symbols }
}

/// Global stdlib cache - parsed once and reused
pub static STDLIB_CACHE: Lazy<StdlibCache> = Lazy::new(parse_stdlib);

/// Get the global stdlib cache
pub fn get_stdlib_cache() -> &'static StdlibCache {
    &STDLIB_CACHE
}
