use std::sync::Arc;

use camino::Utf8PathBuf;
use dashmap::DashMap;
use kola_syntax::{
    lexer::{LexInput, tokenize},
    parser::{ParseInput, ParseOutput, parse},
};
use kola_utils::interner::{PathInterner, StrInterner};
use tokio::sync::RwLock;

use kola_span::{Report, Source};
use tower_lsp_server::{Client, LanguageServer, UriExt, jsonrpc, lsp_types::*};

use crate::token::{LEGEND, to_lsp_tokens};

pub struct Server {
    client: Client,
    interner: RwLock<PathInterner>,
    sources: DashMap<Utf8PathBuf, Source>,
    outputs: DashMap<Utf8PathBuf, ParseOutput>,
}

impl Server {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            interner: RwLock::new(PathInterner::new()),
            sources: DashMap::new(),
            outputs: DashMap::new(),
        }
    }

    fn uri_to_path(uri: &Uri) -> Option<Utf8PathBuf> {
        uri.to_file_path()
            .and_then(|path| Utf8PathBuf::from_path_buf(path.into_owned()).ok())
    }

    fn report_to_diags(report: Report, source: &Source) -> Vec<Diagnostic> {
        report
            .diagnostics
            .into_iter()
            .filter_map(
                |kola_span::Diagnostic {
                     message,
                     severity,
                     loc,
                     ..
                 }| {
                    // Convert loc (span) to LSP Range
                    let (start_line, start_line_num, start_byte_col) =
                        source.get_byte_line(loc.span.start)?;
                    let (end_line, end_line_num, end_byte_col) =
                        source.get_byte_line(loc.span.end)?;

                    let start_line_text = source.get_line_text(start_line)?;
                    let end_line_text = source.get_line_text(end_line)?;

                    let start_utf16_col =
                        start_line_text[..start_byte_col].encode_utf16().count() as u32;
                    let end_utf16_col = end_line_text[..end_byte_col].encode_utf16().count() as u32;

                    let range = Range {
                        start: Position {
                            line: start_line_num as u32,
                            character: start_utf16_col,
                        },
                        end: Position {
                            line: end_line_num as u32,
                            character: end_utf16_col,
                        },
                    };

                    let severity = match severity {
                        kola_span::Severity::Error => DiagnosticSeverity::ERROR,
                        kola_span::Severity::Warning => DiagnosticSeverity::WARNING,
                        kola_span::Severity::Info => DiagnosticSeverity::INFORMATION,
                    };

                    Some(Diagnostic {
                        range,
                        severity: Some(severity),
                        source: Some("kola".to_string()),
                        message,
                        ..Default::default()
                    })
                },
            )
            .collect()
    }

    pub async fn on_change(&self, uri: Uri, version: i32, text: String) {
        let Some(path) = Self::uri_to_path(&uri) else {
            self.client
                .log_message(MessageType::ERROR, "Invalid URI")
                .await;
            return;
        };
        let text: Arc<str> = text.into();

        let source_id = self.interner.write().await.intern(path.as_path());
        let source = Source::from(text);

        let mut interner = StrInterner::new();
        let mut report = Report::new();

        let Some(tokens) = tokenize(LexInput::new(source_id, source.text()), &mut report) else {
            let diags = Self::report_to_diags(report, &source);
            self.client
                .publish_diagnostics(uri, diags, Some(version))
                .await;
            return;
        };

        let output = parse(
            ParseInput::new(source_id, tokens, source.len()),
            &mut interner,
            &mut report,
        );

        if !report.is_empty() {
            let diags = Self::report_to_diags(report, &source);

            self.client
                .publish_diagnostics(uri, diags, Some(version))
                .await;
        }

        self.sources.insert(path.clone(), source);
        self.outputs.insert(path, output);
    }

    pub async fn on_semantic_tokens_full(&self, uri: Uri) -> Option<SemanticTokensResult> {
        let path = Self::uri_to_path(&uri)?;

        let source = self.sources.get(&path)?;
        let output = self.outputs.get(&path)?;

        let data = to_lsp_tokens(&output.tokens, &*source);

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }

    pub async fn on_semantic_tokens_range(
        &self,
        uri: Uri,
        range: Range,
    ) -> Option<SemanticTokensRangeResult> {
        let path = Self::uri_to_path(&uri)?;

        let source = self.sources.get(&path)?;
        let output = self.outputs.get(&path)?;

        let data = to_lsp_tokens(&output.tokens, &*source);

        Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }
}

impl LanguageServer for Server {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("kola".to_owned()),
                                        scheme: Some("file".to_owned()),
                                        pattern: Some("**/*.kl".to_owned()),
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        dbg!("Server initialized");
        self.client
            .log_message(MessageType::INFO, "Server initialized")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        dbg!("File opened: {:?}", &params.text_document.uri);
        self.client
            .log_message(MessageType::INFO, "File opened")
            .await;

        let DidOpenTextDocumentParams {
            text_document:
                TextDocumentItem {
                    uri,
                    language_id: _,
                    version,
                    text,
                },
        } = params;

        self.on_change(uri, version, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        dbg!("File changed: {:?}", &params.text_document.uri);
        self.client
            .log_message(MessageType::INFO, "File changed")
            .await;

        let DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier { uri, version },
            mut content_changes,
        } = params;

        let text = content_changes.remove(0).text;

        self.on_change(uri, version, text).await;
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        dbg!("File saved");
        self.client
            .log_message(MessageType::INFO, "File saved")
            .await;
    }

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {
        dbg!("File closed");
        self.client
            .log_message(MessageType::INFO, "File closed")
            .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        dbg!(
            "Semantic tokens full request for: {:?}",
            &params.text_document.uri
        );
        let uri = params.text_document.uri;

        Ok(self.on_semantic_tokens_full(uri).await)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> jsonrpc::Result<Option<SemanticTokensRangeResult>> {
        dbg!(
            "Semantic tokens range request for: {:?}, range: {:?}",
            &params.text_document.uri,
            &params.range
        );
        let SemanticTokensRangeParams {
            text_document: TextDocumentIdentifier { uri },
            range,
            ..
        } = params;

        Ok(self.on_semantic_tokens_range(uri, range).await)
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        dbg!("Server shutdown requested");
        Ok(())
    }
}
