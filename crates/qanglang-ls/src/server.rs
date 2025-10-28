use std::collections::HashMap;
use std::sync::Arc;

use log::debug;
use qanglang_core::SourceMap;
use serde_json::Value;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::SemanticTokenType;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analyzer::{AnalysisResult, analyze};
use crate::hover_utils::{find_node_at_offset, format_hover_info, get_declaration_range};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::CLASS,     // 0: class names
    SemanticTokenType::TYPE,      // 1: type names
    SemanticTokenType::PARAMETER, // 2: function/method parameters
    SemanticTokenType::VARIABLE,  // 3: local variables
    SemanticTokenType::PROPERTY,  // 4: object properties / class fields
    SemanticTokenType::FUNCTION,  // 5: function names
    SemanticTokenType::METHOD,    // 6: method names
    SemanticTokenType::KEYWORD,   // 7: keywords (class, func, let, etc.)
    SemanticTokenType::STRING,    // 8: string literals
    SemanticTokenType::NUMBER,    // 9: number literals
    SemanticTokenType::OPERATOR,  // 10: operators (+, -, *, etc.)
    SemanticTokenType::COMMENT,   // 11: comments (future)
];

/// Helper to get token type index from SemanticTokenType
pub(crate) fn token_type_index(token_type: SemanticTokenType) -> u32 {
    LEGEND_TYPE.iter().position(|t| t == &token_type).unwrap() as u32
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
    /// Cache of analysis results, keyed by document URI
    analysis_cache: Arc<RwLock<HashMap<Url, Arc<AnalysisResult>>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                // inlay_hint_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: None,
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        // save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        //     include_text: Some(true),
                        // })),
                        ..Default::default()
                    },
                )),
                // completion_provider: Some(CompletionOptions {
                //     resolve_provider: Some(false),
                //     trigger_characters: Some(vec![".".to_string()]),
                //     work_done_progress_options: Default::default(),
                //     all_commit_characters: None,
                //     completion_item: None,
                // }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("qanglang".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.to_vec(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
            language_id: "".to_string(), // not used
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let text = params
            .content_changes
            .get_mut(0)
            .map(|d| std::mem::take(&mut d.text))
            .unwrap();

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text,
            version: params.text_document.version,
            language_id: "".to_string(), // not used
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.on_change(TextDocumentItem {
                uri: params.text_document.uri,
                text,
                version: 0,                  // Save doesn't provide version
                language_id: "".to_string(), // not used
            })
            .await
        }
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        debug!(
            "Go to definition request at {}:{}",
            position.line, position.character
        );

        // Get cached analysis
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for goto definition");
                return Ok(None);
            }
        };
        drop(cache);

        // Convert LSP position to byte offset
        let offset = match analysis
            .source_map
            .position_to_offset(position.line, position.character)
        {
            Some(offset) => offset,
            None => {
                debug!("Invalid position for goto definition");
                return Ok(None);
            }
        };

        debug!("Looking for definition at offset {}", offset);

        // Find node at position - this already resolves to the declaration via symbol table
        let node_info = find_node_at_offset(&analysis, offset);

        if let Some(info) = node_info {
            debug!(
                "Found definition node: {:?}, node_id: {:?}",
                info.kind, info.node_id
            );

            // Get the actual declaration's range (not the reference's range from info.range)
            let decl_range = match get_declaration_range(&analysis, info.node_id) {
                Some(range) => range,
                None => {
                    debug!("Could not get declaration range for node_id: {:?}", info.node_id);
                    // Fall back to using the range from info (which might be a reference)
                    info.range
                }
            };

            let location = Location {
                uri: uri.clone(),
                range: decl_range,
            };

            debug!("Returning location: {:?}", location);
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }

        debug!("No definition found at position");
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        debug!(
            "References request at {}:{}",
            position.line, position.character
        );

        // Get cached analysis
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for references");
                return Ok(None);
            }
        };
        drop(cache);

        // Convert LSP position to byte offset
        let offset = match analysis
            .source_map
            .position_to_offset(position.line, position.character)
        {
            Some(offset) => offset,
            None => {
                debug!("Invalid position for references");
                return Ok(None);
            }
        };

        debug!("Looking for references at offset {}", offset);

        // Find the declaration node at the cursor position
        let node_info = find_node_at_offset(&analysis, offset);

        if let Some(info) = node_info {
            debug!(
                "Found declaration node: {:?}, node_id: {:?}",
                info.kind, info.node_id
            );

            let decl_node_id = info.node_id;
            let mut locations = Vec::new();
            let mut seen_ranges: std::collections::HashSet<(u32, u32, u32, u32)> = std::collections::HashSet::new();

            // Optionally include the declaration itself
            if params.context.include_declaration {
                let range_key = (info.range.start.line, info.range.start.character,
                                 info.range.end.line, info.range.end.character);
                seen_ranges.insert(range_key);

                locations.push(Location {
                    uri: uri.clone(),
                    range: info.range,
                });
                debug!("Added declaration location: {:?}", info.range);
            }

            // Find all references to this declaration by iterating through the symbol table
            let mut ref_count = 0;
            for (ref_id, symbol_info) in analysis.symbol_table.all_resolutions() {
                if symbol_info.decl_node_id == decl_node_id {
                    // This is a reference to our declaration
                    // Get the span of the reference node
                    let ref_node = analysis.nodes.get_identifier_node(*ref_id);
                    let ref_span = ref_node.node.span;

                    let range = Range {
                        start: Position {
                            line: analysis.source_map.get_line_number(ref_span.start) - 1,
                            character: analysis.source_map.get_column_number(ref_span.start) - 1,
                        },
                        end: Position {
                            line: analysis.source_map.get_line_number(ref_span.end) - 1,
                            character: analysis.source_map.get_column_number(ref_span.end) - 1,
                        },
                    };

                    // Skip if we've already seen this range (deduplication)
                    let range_key = (range.start.line, range.start.character,
                                     range.end.line, range.end.character);
                    if seen_ranges.contains(&range_key) {
                        debug!("Skipping duplicate range: {:?}", range);
                        continue;
                    }
                    seen_ranges.insert(range_key);

                    debug!("Found reference at {:?}", range);
                    locations.push(Location {
                        uri: uri.clone(),
                        range,
                    });
                    ref_count += 1;
                }
            }

            debug!("Found {} reference(s) total", ref_count);
            debug!("Total locations (including decl if requested): {}", locations.len());
            return Ok(Some(locations));
        }

        debug!("No references found at position");
        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        debug!(
            "Rename request at {}:{} to '{}'",
            position.line, position.character, new_name
        );

        // Get cached analysis
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for rename");
                return Ok(None);
            }
        };
        drop(cache);

        // Convert LSP position to byte offset
        let offset = match analysis
            .source_map
            .position_to_offset(position.line, position.character)
        {
            Some(offset) => offset,
            None => {
                debug!("Invalid position for rename");
                return Ok(None);
            }
        };

        debug!("Looking for symbol to rename at offset {}", offset);

        // Find the declaration node at the cursor position
        let node_info = find_node_at_offset(&analysis, offset);

        if let Some(info) = node_info {
            debug!("Found symbol to rename: {:?}", info.kind);

            let decl_node_id = info.node_id;
            let mut text_edits = Vec::new();
            let mut seen_ranges: std::collections::HashSet<(u32, u32, u32, u32)> = std::collections::HashSet::new();

            // Include the declaration itself
            let decl_range_key = (info.range.start.line, info.range.start.character,
                                  info.range.end.line, info.range.end.character);
            seen_ranges.insert(decl_range_key);

            text_edits.push(TextEdit {
                range: info.range,
                new_text: new_name.clone(),
            });

            // Find all references to this declaration
            for (ref_id, symbol_info) in analysis.symbol_table.all_resolutions() {
                if symbol_info.decl_node_id == decl_node_id {
                    // This is a reference to our declaration
                    // Get the span of the reference node
                    let ref_node = analysis.nodes.get_identifier_node(*ref_id);
                    let ref_span = ref_node.node.span;

                    let range = Range {
                        start: Position {
                            line: analysis.source_map.get_line_number(ref_span.start) - 1,
                            character: analysis.source_map.get_column_number(ref_span.start) - 1,
                        },
                        end: Position {
                            line: analysis.source_map.get_line_number(ref_span.end) - 1,
                            character: analysis.source_map.get_column_number(ref_span.end) - 1,
                        },
                    };

                    // Skip if we've already seen this range (deduplication)
                    let range_key = (range.start.line, range.start.character,
                                     range.end.line, range.end.character);
                    if seen_ranges.contains(&range_key) {
                        debug!("Skipping duplicate range for rename: {:?}", range);
                        continue;
                    }
                    seen_ranges.insert(range_key);

                    text_edits.push(TextEdit {
                        range,
                        new_text: new_name.clone(),
                    });
                }
            }

            debug!("Created {} text edit(s) for rename", text_edits.len());

            // Create workspace edit
            let mut changes = HashMap::new();
            changes.insert(uri, text_edits);

            return Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }));
        }

        debug!("No symbol found to rename at position");
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        debug!("Hover request at {}:{}", position.line, position.character);

        // Get cached analysis
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for hover");
                return Ok(None);
            }
        };
        drop(cache); // Release lock

        // Convert LSP position to byte offset
        let offset = match analysis
            .source_map
            .position_to_offset(position.line, position.character)
        {
            Some(offset) => offset,
            None => {
                debug!("Invalid position for hover");
                return Ok(None);
            }
        };

        debug!("Looking for node at offset {}", offset);

        // Find node at position
        let node_info = find_node_at_offset(&analysis, offset);

        if let Some(info) = node_info {
            debug!("Found node: {:?}", info.kind);
            let hover_text = format_hover_info(&analysis, &info);

            return Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(hover_text)),
                range: Some(info.range),
            }));
        }

        debug!("No node found at position");
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        debug!("Document symbol request for: {}", uri);

        // Get cached analysis (reuse cache from hover implementation)
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for document symbols");
                return Ok(None);
            }
        };
        drop(cache);

        // Collect symbols from AST
        let symbols = crate::symbol_collector::collect_document_symbols(&analysis);

        if symbols.is_empty() {
            debug!("No symbols collected");
            return Ok(None);
        }

        debug!("Returning {} symbols", symbols.len());
        // Return hierarchical document symbols
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        debug!("Semantic tokens full request for: {}", uri);

        // Get cached analysis
        let cache = self.analysis_cache.read().await;
        let analysis = match cache.get(&uri) {
            Some(analysis) => analysis.clone(),
            None => {
                debug!("No analysis available for semantic tokens");
                return Ok(None);
            }
        };
        drop(cache);

        // Collect semantic tokens from AST
        let tokens = crate::semantic_tokens::collect_semantic_tokens(&analysis);

        debug!("Collected {} semantic tokens", tokens.len());

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        debug!("Semantic tokens range request");

        // For now, just delegate to full document
        // Can optimize later to only process the requested range
        let full_params = SemanticTokensParams {
            text_document: params.text_document,
            work_done_progress_params: params.work_done_progress_params,
            partial_result_params: params.partial_result_params,
        };

        match self.semantic_tokens_full(full_params).await? {
            Some(SemanticTokensResult::Tokens(tokens)) => {
                Ok(Some(SemanticTokensRangeResult::Tokens(tokens)))
            }
            _ => Ok(None),
        }
    }
}

impl Backend {
    async fn on_change(&self, document: TextDocumentItem) {
        let document_filepath = match document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => {
                return;
            }
        };
        debug!("=== Starting analysis for: {} ===", document.uri);

        let source_map = SourceMap::new(document.text, document_filepath);
        let source_map = Arc::new(source_map);
        let mut diagnostics = Vec::new();

        let errors = match analyze(source_map.clone()) {
            Ok(result) => {
                debug!("✅ Analysis succeeded - no errors found");

                // Cache the successful analysis
                let result = Arc::new(result);
                self.analysis_cache
                    .write()
                    .await
                    .insert(document.uri.clone(), result.clone());

                Vec::new() // no diagnostics
            }
            Err(error) => {
                debug!("❌ Analysis failed");
                debug!("Error count: {}", error.all().len());

                // Remove cached analysis on error
                self.analysis_cache.write().await.remove(&document.uri);

                error.into_errors()
            }
        };

        for error in errors {
            let start_line = source_map.get_line_number(error.span.start) - 1;
            let start_char = source_map.get_column_number(error.span.start) - 1;
            let end_line = source_map.get_line_number(error.span.end) - 1;
            let end_char = source_map.get_column_number(error.span.end) - 1;

            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: start_line,
                        character: start_char,
                    },
                    end: Position {
                        line: end_line,
                        character: end_char,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: None,
                message: error.message,
                related_information: None,
                tags: None,
                data: None,
            });
        }

        self.client
            .publish_diagnostics(document.uri, diagnostics, Some(document.version))
            .await;
        debug!("=== Analysis complete ===");
    }
}

pub fn run_language_server() {
    env_logger::Builder::from_default_env()
        .target(env_logger::Target::Stderr)
        .init();
    debug!("Starting QangLang Language Server...");

    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::build(|client| Backend {
            client,
            analysis_cache: Arc::new(RwLock::new(HashMap::new())),
        })
        .finish();

        debug!("Starting server...");
        Server::new(stdin, stdout, socket).serve(service).await;
        debug!("Server stopped");
    });
}
