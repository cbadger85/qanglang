use crate::analyzer::AnalysisResult;
use qanglang_core::nodes::*;
use qanglang_core::{AstNodeArena, SourceMap, StringInterner, TypedNodeRef};
use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};

/// Collect all document symbols from the analysis result
pub fn collect_document_symbols(analysis: &AnalysisResult) -> Vec<DocumentSymbol> {
    use log::debug;

    let module = analysis.nodes.get_program_node(analysis.root_module_id);

    let mut collector = SymbolCollector {
        symbols: Vec::new(),
        nodes: &analysis.nodes,
        strings: &analysis.strings,
        source_map: &analysis.source_map,
    };

    collector.collect_from_module(module);

    debug!("Collected {} document symbols", collector.symbols.len());

    collector.symbols
}

struct SymbolCollector<'a> {
    symbols: Vec<DocumentSymbol>,
    nodes: &'a AstNodeArena,
    strings: &'a StringInterner,
    source_map: &'a SourceMap,
}

impl<'a> SymbolCollector<'a> {
    fn collect_from_module(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);
                if let Some(symbol) = self.process_decl(decl) {
                    self.symbols.push(symbol);
                }
            }
        }
    }

    fn process_decl(&self, decl: TypedNodeRef<DeclNode>) -> Option<DocumentSymbol> {
        use log::debug;

        match decl.node {
            DeclNode::Class(class) => {
                debug!("Processing class symbol");
                Some(self.process_class(TypedNodeRef::new(decl.id, class)))
            }
            DeclNode::Function(func) => {
                debug!("Processing function symbol");
                Some(self.process_function_decl(TypedNodeRef::new(decl.id, func)))
            }
            DeclNode::Variable(var) => {
                debug!("Processing variable symbol");
                Some(self.process_variable(TypedNodeRef::new(decl.id, var)))
            }
            DeclNode::Lambda(lambda) => {
                debug!("Processing lambda symbol");
                Some(self.process_lambda_decl(TypedNodeRef::new(decl.id, lambda)))
            }
            DeclNode::Module(import) => {
                debug!("Processing import symbol");
                Some(self.process_import(TypedNodeRef::new(decl.id, import)))
            }
            DeclNode::Stmt(_) => {
                debug!("Skipping statement (not a symbol)");
                None
            }
        }
    }

    fn process_class(&self, class: TypedNodeRef<ClassDeclNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(class.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        // Collect class members (methods and fields)
        let mut children = Vec::new();
        let length = self.nodes.array.size(class.node.members);

        for i in 0..length {
            if let Some(member_id) = self.nodes.array.get_node_id_at(class.node.members, i) {
                let member = self.nodes.get_class_member_node(member_id);
                if let Some(symbol) = self.process_class_member(member) {
                    children.push(symbol);
                }
            }
        }

        #[allow(deprecated)]
        DocumentSymbol {
            name: name.clone(),
            detail: class.node.superclass.map(|superclass_id| {
                let superclass_node = self.nodes.get_identifier_node(superclass_id);
                format!(
                    "extends {}",
                    self.strings.get_string(superclass_node.node.name)
                )
            }),
            kind: SymbolKind::CLASS,
            range: self.span_to_range(class.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
            tags: None,
            deprecated: None,
        }
    }

    fn process_class_member(
        &self,
        member: TypedNodeRef<ClassMemberNode>,
    ) -> Option<DocumentSymbol> {
        match member.node {
            ClassMemberNode::Method(method) => {
                Some(self.process_method(TypedNodeRef::new(member.id, method)))
            }
            ClassMemberNode::Field(field) => {
                Some(self.process_field(TypedNodeRef::new(member.id, field)))
            }
        }
    }

    fn process_method(&self, method: TypedNodeRef<FunctionExprNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(method.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        // Collect parameters for detail
        let param_count = self.nodes.array.size(method.node.parameters);
        let detail = format!("({} parameters)", param_count);

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: Some(detail),
            kind: SymbolKind::METHOD,
            range: self.span_to_range(method.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None, // Could add parameters as children later
            tags: None,
            deprecated: None,
        }
    }

    fn process_field(&self, field: TypedNodeRef<FieldDeclNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(field.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: None,
            kind: SymbolKind::FIELD,
            range: self.span_to_range(field.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None,
            tags: None,
            deprecated: None,
        }
    }

    fn process_function_decl(&self, func: TypedNodeRef<FunctionDeclNode>) -> DocumentSymbol {
        let func_expr = self.nodes.get_func_expr_node(func.node.function);
        let name_node = self.nodes.get_identifier_node(func_expr.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        let param_count = self.nodes.array.size(func_expr.node.parameters);
        let detail = format!("({} parameters)", param_count);

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: Some(detail),
            kind: SymbolKind::FUNCTION,
            range: self.span_to_range(func_expr.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None, // Could add parameters as children
            tags: None,
            deprecated: None,
        }
    }

    fn process_lambda_decl(&self, lambda: TypedNodeRef<LambdaDeclNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(lambda.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        #[allow(deprecated)]
        DocumentSymbol {
            name: format!("λ {}", name),
            detail: Some("lambda".to_string()),
            kind: SymbolKind::FUNCTION,
            range: self.span_to_range(lambda.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None,
            tags: None,
            deprecated: None,
        }
    }

    fn process_variable(&self, var: TypedNodeRef<VariableDeclNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(var.node.target);
        let name = self.strings.get_string(name_node.node.name).to_string();

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: None,
            kind: SymbolKind::VARIABLE,
            range: self.span_to_range(var.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None,
            tags: None,
            deprecated: None,
        }
    }

    fn process_import(&self, import: TypedNodeRef<ImportModuleDeclNode>) -> DocumentSymbol {
        let name_node = self.nodes.get_identifier_node(import.node.name);
        let name = self.strings.get_string(name_node.node.name).to_string();

        #[allow(deprecated)]
        DocumentSymbol {
            name: format!("import {}", name),
            detail: Some(self.strings.get_string(import.node.path).to_string()),
            kind: SymbolKind::MODULE,
            range: self.span_to_range(import.node.span),
            selection_range: self.span_to_range(name_node.node.span),
            children: None,
            tags: None,
            deprecated: None,
        }
    }

    fn span_to_range(&self, span: SourceSpan) -> Range {
        Range {
            start: Position {
                line: (self.source_map.get_line_number(span.start) - 1) as u32,
                character: (self.source_map.get_column_number(span.start) - 1) as u32,
            },
            end: Position {
                line: (self.source_map.get_line_number(span.end) - 1) as u32,
                character: (self.source_map.get_column_number(span.end) - 1) as u32,
            },
        }
    }
}
