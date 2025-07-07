use kola_span::{Located, Source};
use kola_syntax::token::{Literal, SemanticToken};
use tower_lsp_server::lsp_types::{SemanticToken as LspToken, SemanticTokenType as LspTokenType};

pub const LEGEND: &[LspTokenType] = &[
    LspTokenType::KEYWORD,
    LspTokenType::STRING,
    LspTokenType::NUMBER,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SemanticTokenKind {
    Kw = 0,
    Str = 1,
    Num = 2,
}

impl SemanticTokenKind {
    pub fn from_token(token: SemanticToken) -> Option<Self> {
        match token {
            SemanticToken::Kw(_) => Some(Self::Kw),
            SemanticToken::Literal(Literal::Str) => Some(Self::Str),
            SemanticToken::Literal(Literal::Num) => Some(Self::Num),
            _ => None,
        }
    }

    pub fn from_located_token((token, loc): &Located<SemanticToken>) -> Option<Located<Self>> {
        let this = Self::from_token(token.clone())?;
        Some((this, *loc))
    }
}

pub fn to_lsp_tokens(tokens: &[Located<SemanticToken>], source: &Source) -> Vec<LspToken> {
    let mut tokens = tokens
        .into_iter()
        .filter_map(SemanticTokenKind::from_located_token)
        .collect::<Vec<_>>();

    tokens.sort_by_key(|(_, loc)| loc.span.start);

    let mut result = Vec::with_capacity(tokens.len());
    let mut prev_line = 0;
    let mut prev_col = 0;

    for (kind, loc) in tokens {
        // Get line, line number, and byte column for the token start
        let Some((line, line_number, byte_col)) = source.get_byte_line(loc.span.start) else {
            continue;
        };
        let Some(line_text) = source.get_line_text(line) else {
            continue;
        };

        // Convert byte column to UTF-16 column
        let Some(prefix) = line_text.get(..byte_col) else {
            continue;
        };
        let utf16_col = prefix.encode_utf16().count() as u32;

        // Compute delta_line and delta_start
        let delta_line = (line_number as u32).saturating_sub(prev_line);
        let delta_start = if delta_line == 0 {
            utf16_col.saturating_sub(prev_col)
        } else {
            utf16_col
        };

        // Compute token length in UTF-16 code units
        let token_text = &source.text()[loc.span.start..loc.span.end];
        let length = token_text.encode_utf16().count() as u32;

        result.push(LspToken {
            delta_line,
            delta_start,
            length,
            token_type: kind as u32,
            token_modifiers_bitset: 0,
        });

        prev_line = line_number as u32;
        prev_col = utf16_col;
    }

    result
}
