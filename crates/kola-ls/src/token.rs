use kola_span::{Located, Source};
use kola_syntax::token::{Ctrl, Literal, SemanticToken, Symbol};
use tower_lsp_server::{
    Client,
    ls_types::{MessageType, SemanticToken as LspToken, SemanticTokenType as LspTokenType},
};

pub const LEGEND: &[LspTokenType] = &[
    LspTokenType::KEYWORD,
    LspTokenType::STRING,
    LspTokenType::NUMBER,
    LspTokenType::OPERATOR,
    LspTokenType::VARIABLE,
    LspTokenType::MACRO,
    LspTokenType::NAMESPACE,
    LspTokenType::DECORATOR,
    LspTokenType::FUNCTION,
    LspTokenType::INTERFACE,
    LspTokenType::TYPE,
    LspTokenType::TYPE_PARAMETER,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SemanticTokenKind {
    Kw = 0,
    Str = 1,
    Num = 2,
    Op = 3,
    Var = 4,
    Macro = 5,
    Namespace = 6,
    Decorator = 7,
    Function = 8,
    Interface = 9,
    Type = 10,
    TypeParameter = 11,
}

impl SemanticTokenKind {
    pub fn from_token(token: SemanticToken) -> Option<Self> {
        match token {
            SemanticToken::Symbol(Symbol::Functor) => Some(Self::Function),
            SemanticToken::Symbol(Symbol::ModuleType) => Some(Self::Interface),
            SemanticToken::Symbol(Symbol::Module) => Some(Self::Namespace),
            SemanticToken::Symbol(Symbol::Kind)
            | SemanticToken::Symbol(Symbol::Effect)
            | SemanticToken::Symbol(Symbol::Type) => Some(Self::Type),
            SemanticToken::Symbol(Symbol::TypeVar) => Some(Self::TypeParameter),
            SemanticToken::Symbol(Symbol::Value) | SemanticToken::Symbol(Symbol::Tag) => {
                Some(Self::Var)
            }
            SemanticToken::Literal(Literal::Str) => Some(Self::Str),
            SemanticToken::Literal(Literal::Num)
            | SemanticToken::Literal(Literal::Bool)
            | SemanticToken::Literal(Literal::Unit) => Some(Self::Num),
            SemanticToken::Kw(_) => Some(Self::Kw),
            SemanticToken::Op(_) => Some(Self::Op),
            SemanticToken::Ctrl(Ctrl::Arrow) | SemanticToken::Ctrl(Ctrl::DoubleArrow) => {
                Some(Self::Macro)
            }
            SemanticToken::Ctrl(Ctrl::At) => Some(Self::Decorator),
            SemanticToken::Ctrl(_) => Some(Self::Op),

            _ => None,
        }
    }

    pub fn from_located_token((token, loc): &Located<SemanticToken>) -> Option<Located<Self>> {
        let this = Self::from_token(token.clone())?;
        Some((this, *loc))
    }
}

pub fn to_lsp_tokens(
    client: &Client,
    tokens: &[Located<SemanticToken>],
    source: &Source,
) -> Vec<LspToken> {
    dbg!(tokens);

    let mut tokens = tokens
        .into_iter()
        .filter_map(SemanticTokenKind::from_located_token)
        .collect::<Vec<_>>();

    tokens.sort_by_key(|(_, loc)| loc.span.start);

    let len = tokens.len();
    let mut result = Vec::with_capacity(len);
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

    if result.len() < len {
        client.log_message(
            MessageType::WARNING,
            format!("Expected {} tokens, got {}", len, result.len()),
        );
    }

    result
}
