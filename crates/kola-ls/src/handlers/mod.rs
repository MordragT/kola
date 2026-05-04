use crate::server::Server;
use tower_lsp_server::ls_types as ls;

mod semantic_tokens;
mod text_document;

pub use semantic_tokens::{LEGEND, on_semantic_tokens_full, on_semantic_tokens_range};
pub use text_document::{on_changed, on_closed, on_opened, on_saved};

pub async fn on_initilized(server: &Server, _params: ls::InitializedParams) {
    dbg!("Server initialized");
    server
        .client
        .log_message(ls::MessageType::INFO, "Server initialized")
        .await;
}
