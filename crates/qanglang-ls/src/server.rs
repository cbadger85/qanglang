use std::sync::Arc;

use qanglang_core::SourceMap;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
// use tower_lsp::lsp_types::SemanticTokenType;
use log::info;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analyzer::analyze;

// pub const LEGEND_TYPE: &[SemanticTokenType] = &[
//     SemanticTokenType::FUNCTION,
//     SemanticTokenType::CLASS,
//     SemanticTokenType::STRUCT,
//     SemanticTokenType::METHOD,
//     SemanticTokenType::VARIABLE,
//     SemanticTokenType::STRING,
//     SemanticTokenType::NUMBER,
//     SemanticTokenType::KEYWORD,
//     SemanticTokenType::OPERATOR,
//     SemanticTokenType::PARAMETER,
// ];

#[derive(Debug)]
pub struct Backend {
    client: Client,
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
                // semantic_tokens_provider: Some(
                //     SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                //         SemanticTokensRegistrationOptions {
                //             text_document_registration_options: {
                //                 TextDocumentRegistrationOptions {
                //                     document_selector: Some(vec![DocumentFilter {
                //                         language: Some("qanglang".to_string()),
                //                         scheme: Some("file".to_string()),
                //                         pattern: None,
                //                     }]),
                //                 }
                //             },
                //             semantic_tokens_options: SemanticTokensOptions {
                //                 work_done_progress_options: WorkDoneProgressOptions::default(),
                //                 legend: SemanticTokensLegend {
                //                     token_types: LEGEND_TYPE.into(),
                //                     token_modifiers: vec![],
                //                 },
                //                 range: Some(true),
                //                 full: Some(SemanticTokensFullOptions::Bool(true)),
                //             },
                //             static_registration_options: StaticRegistrationOptions::default(),
                //         },
                //     ),
                // ),
                // definition: Some(GotoCapability::default()),
                // definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                // rename_provider: Some(OneOf::Left(true)),
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
}

impl Backend {
    async fn on_change(&self, document: TextDocumentItem) {
        let document_filepath = match document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => {
                return;
            }
        };
        info!("=== Starting analysis for: {} ===", document.uri);

        let mut source_map = SourceMap::new(document.text);
        source_map.set_path(document_filepath);
        let source_map = Arc::new(source_map);
        let mut diagnostics = Vec::new();

        let errors = match analyze(source_map.clone()) {
            Ok(_) => {
                info!("✅ Analysis succeeded - no errors found");
                Vec::new()
            }
            Err(error) => {
                info!("❌ Analysis failed");
                info!("Error count: {}", error.all().len());
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
        info!("=== Analysis complete ===");
    }
}

pub fn run_language_server() {
    // Initialize logging for the language server first
    // Ensure logs go to stderr, not stdout (which LSP uses)
    env_logger::Builder::from_default_env()
        .target(env_logger::Target::Stderr)
        .init();
    info!("Starting QangLang Language Server...");

    // Create a new tokio runtime for the language server
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::build(|client| Backend { client }).finish();

        info!("Starting server...");
        Server::new(stdin, stdout, socket).serve(service).await;
        info!("Server stopped");
    });
}
