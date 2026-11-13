use std::sync::Arc;

use kola_interner::StrInterner;
use kola_span::{Report, Source};
use kola_syntax::parser::{ParseInput, parse};
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

    // `Source` is `ariadne::Source<&'static str>`, so the document text has to
    // outlive the request. The LSP session is the effective lifetime here.
    let text: &'static str = Box::leak(text.into_boxed_str());
    let source = Source::from(text);

    // Lexing and parsing are synchronous, and `SourceManager` is not `Send`, so
    // the whole compile step is scoped to drop before any `.await`.
    let (report, parse_output) = {
        // Tokenizing resolves imports through the filesystem, and the document
        // may not be on disk yet, so serve it from an in-memory filesystem.
        let mut fs = kola_span::io::MockFileSystem::new();
        fs.add_file(path.clone(), text.to_owned());

        let mut source_manager = kola_span::SourceManager::new(Arc::new(fs));
        let mut interner = StrInterner::default();
        let mut report = Report::new();

        match kola_syntax::lexer::try_tokenize(&mut source_manager, path.as_path()) {
            Ok(output) => {

                let source_id = output.root_id;
                let tokens = output.token_map[&source_id].clone();
                let parse_output =
                    parse(ParseInput::new(source_id, tokens, &mut interner), &mut report);
                (report, Some(parse_output))
            }
            Err(err) => (err, None),
        }
    };

    if !report.is_empty() {
        publish(server, uri.clone(), version, &source, report).await;
    }

    if let Some(parse_output) = parse_output {
        server.outputs.insert(path.clone(), parse_output);
    }
    server.sources.insert(path, source);
}

async fn publish(
    server: &Server,
    uri: ls::Uri,
    version: i32,
    source: &Source,
    report: Report,
) {
    let diags = utils::report_to_diags(report, source);
    server
        .client
        .publish_diagnostics(uri, diags, Some(version))
        .await;
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
