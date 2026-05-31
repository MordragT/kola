use camino::Utf8PathBuf;
use dashmap::DashMap;
use kola_syntax::parser::ParseOutput;
use kola_utils::interner::PathInterner;
use tokio::sync::RwLock;

use kola_span::Source;
use tower_lsp_server::{Client, LanguageServer, jsonrpc, ls_types::*};

use crate::handlers;

pub struct Server {
    pub client: Client,
    pub interner: RwLock<PathInterner>,
    pub sources: DashMap<Utf8PathBuf, Source>,
    pub outputs: DashMap<Utf8PathBuf, ParseOutput>,
}

impl Server {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            interner: RwLock::new(PathInterner::default()),
            sources: DashMap::new(),
            outputs: DashMap::new(),
        }
    }
}

impl LanguageServer for Server {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
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
                                    token_types: handlers::LEGEND.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // inlay_hint_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        handlers::on_initilized(self, params).await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        handlers::on_opened(self, params).await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        handlers::on_changed(self, params).await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        handlers::on_saved(self, params).await
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        handlers::on_closed(self, params).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        Ok(handlers::on_semantic_tokens_full(self, params).await)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> jsonrpc::Result<Option<SemanticTokensRangeResult>> {
        Ok(handlers::on_semantic_tokens_range(self, params).await)
    }

    // async fn inlay_hint(&self, params: InlayHintParams) -> jsonrpc::Result<Option<Vec<InlayHint>>> {
    //     Ok(handlers::on_inlay_hint(self, params).await)
    // }

    // async fn inlay_hint_resolve(&self, params: InlayHint) -> jsonrpc::Result<InlayHint> {
    //     Ok(handlers::on_inlay_hint_resolve(self, params).await)
    // }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}
