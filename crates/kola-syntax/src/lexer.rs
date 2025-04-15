use chumsky::prelude::*;

use crate::{
    error::{SyntaxError, SyntaxErrors},
    span::{Span, Spanned},
    token::{Delimiter, Op, Token, Tokens},
};

pub struct TokenizeResult<'a> {
    pub tokens: Option<Tokens<'a>>,
    pub errors: SyntaxErrors,
}

pub fn tokenize(input: &str) -> TokenizeResult<'_> {
    let lexer = lexer();
    let (tokens, errors) = lexer.parse(input).into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();
    TokenizeResult { tokens, errors }
}

pub type Extra<'src> = extra::Full<Rich<'src, char, Span>, (), ()>;

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, Extra<'src>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num);

    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    let character = just('\'')
        .ignore_then(any().filter(|c| *c != '\\' && *c != '\'').or(escape))
        .then_ignore(just('\''))
        .map(Token::Char);

    let string = just('"')
        .ignore_then(
            any()
                .filter(|c| *c != '\\' && *c != '"')
                .or(escape)
                .repeated()
                .to_slice(),
        )
        .then_ignore(just('"'))
        .map(Token::Str);

    // Lexer prioritizes the first defined token when ambiguous
    // therefore longer tokens must be defined before shorter ones

    // Multi-character operators
    let multi_op = choice((
        just("+=").to(Op::AddAssign),
        just("-=").to(Op::SubAssign),
        just("*=").to(Op::MulAssign),
        just("/=").to(Op::DivAssign),
        just("%=").to(Op::RemAssign),
        just("<=").to(Op::LessEq),
        just(">=").to(Op::GreaterEq),
        just("==").to(Op::Eq),
        just("!=").to(Op::NotEq),
        // Keywords that are operators
        just("and").to(Op::And),
        just("or").to(Op::Or),
        just("xor").to(Op::Xor),
    ))
    .map(Token::Op);

    // Single-character operators
    let single_op = choice((
        just('=').to(Op::Assign),
        just('+').to(Op::Add),
        just('-').to(Op::Sub),
        just('*').to(Op::Mul),
        just('/').to(Op::Div),
        just('%').to(Op::Rem),
        just('<').to(Op::Less),
        just('>').to(Op::Greater),
        just('!').to(Op::Not),
        just('&').to(Op::Merge),
    ))
    .map(Token::Op);

    // Multi-character control tokens
    let multi_ctrl = choice((
        just("->").to(Token::Arrow),
        just("=>").to(Token::DoubleArrow),
    ));

    // Single-character control tokens
    let single_ctrl = choice((
        just('.').to(Token::Dot),
        just(':').to(Token::Colon),
        just(',').to(Token::Comma),
        just('~').to(Token::Tilde),
        just('|').to(Token::Pipe),
        just('\\').to(Token::Backslash),
        just('_').to(Token::Underscore),
    ));

    // Delimiters group
    let delim = choice((
        just('(').to(Token::Open(Delimiter::Paren)),
        just(')').to(Token::Close(Delimiter::Paren)),
        just('[').to(Token::Open(Delimiter::Bracket)),
        just(']').to(Token::Close(Delimiter::Bracket)),
        just('{').to(Token::Open(Delimiter::Brace)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    // Keywords and identifiers
    let word = text::ident().map(|ident| match ident {
        "type" => Token::Type,
        "fn" => Token::Fn,
        "functor" => Token::Functor,
        "let" => Token::Let,
        "in" => Token::In,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "case" => Token::Case,
        "of" => Token::Of,
        "import" => Token::Import,
        "export" => Token::Export,
        "forall" => Token::Forall,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Symbol(ident),
    });

    let token = choice((
        num,
        character,
        string,
        multi_op,
        multi_ctrl,
        single_op,
        single_ctrl,
        delim,
        word,
    ));

    let comment = just('#')
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|t, e| (t, e.span()))
        .padded_by(comment.repeated())
        .padded() // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
