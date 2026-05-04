use std::sync::Arc;

use kola_span::{Report, Source};
use kola_syntax::{
    lexer::{LexInput, tokenize},
    parser::{ParseInput, parse},
};
use kola_utils::interner::StrInterner;
use tower_lsp_server::ls_types as ls;

use crate::{server::Server, utils};

async fn on_enter(server: &Server, uri: ls::Uri, version: i32, text: String) {
    let Some(path) = utils::uri_to_path(&uri) else {
        server
            .client
            .log_message(ls::MessageType::ERROR, "Invalid URI")
            .await;
        return;
    };
    let text: Arc<str> = text.into();

    let source_id = server.interner.write().await.intern(path.as_path());
    let source = Source::from(text);

    let mut interner = StrInterner::new();
    let mut report = Report::new();

    let Some(tokens) = tokenize(LexInput::new(source_id, source.text()), &mut report) else {
        let diags = utils::report_to_diags(report, &source);
        server
            .client
            .publish_diagnostics(uri, diags, Some(version))
            .await;
        return;
    };

    let output = parse(
        ParseInput::new(source_id, tokens, &mut interner),
        &mut report,
    );

    if !report.is_empty() {
        let diags = utils::report_to_diags(report, &source);

        server
            .client
            .publish_diagnostics(uri, diags, Some(version))
            .await;
    }

    server.sources.insert(path.clone(), source);
    server.outputs.insert(path, output);
}

pub async fn on_opened(server: &Server, params: ls::DidOpenTextDocumentParams) {
    let msg = format!("File opened: {:?}", &params.text_document.uri);
    server.client.log_message(ls::MessageType::INFO, msg).await;

    let ls::DidOpenTextDocumentParams {
        text_document:
            ls::TextDocumentItem {
                uri,
                language_id: _,
                version,
                text,
            },
    } = params;

    on_enter(server, uri, version, text).await;
}

pub async fn on_changed(server: &Server, params: ls::DidChangeTextDocumentParams) {
    let msg = format!("File changed: {:?}", &params.text_document.uri);
    server.client.log_message(ls::MessageType::INFO, msg).await;

    let ls::DidChangeTextDocumentParams {
        text_document: ls::VersionedTextDocumentIdentifier { uri, version },
        mut content_changes,
    } = params;

    let text = content_changes.remove(0).text;

    on_enter(server, uri, version, text).await;
}

pub async fn on_saved(server: &Server, params: ls::DidSaveTextDocumentParams) {
    let msg = format!("File saved: {:?}", &params.text_document.uri);
    server.client.log_message(ls::MessageType::INFO, msg).await;
}

pub async fn on_closed(server: &Server, params: ls::DidCloseTextDocumentParams) {
    let msg = format!("File closed: {:?}", &params.text_document.uri);
    server.client.log_message(ls::MessageType::INFO, msg).await;
}
