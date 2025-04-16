use chumsky::prelude::*;

use crate::{
    error::{SyntaxError, SyntaxErrors},
    span::{Span, Spanned},
    token::{CloseT, CtrlT, KwT, Literal, OpT, OpenT, Token, Tokens},
};

pub struct TokenizeResult<'t> {
    pub tokens: Option<Tokens<'t>>,
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

pub type Extra<'t> = extra::Full<Rich<'t, char, Span>, (), ()>;

/*
 *  ######     #     #     #  #######  ###  #######  #     #
 *  #         # #    #     #     #      #   #     #  ##    #
 *  #        #   #   #     #     #      #   #     #  # #   #
 *  #       #     #  #     #     #      #   #     #  #  #  #
 *  #       #######  #     #     #      #   #     #  #   # #
 *  #       #     #  #     #     #      #   #     #  #    ##
 *  ######  #     #   #####      #     ###  #######  #     #
 *
 * The tokenizer is using an untyped approach to represent tokens.
 * This means there's no compile-time guarantee that all token patterns
 * are properly handled in the lexer and parser.
 *
 * When adding new tokens or modifying existing ones:
 * - Update the Token enum in token.rs
 * - Update the lexer patterns here
 * - Update the parser to handle the new token
 * - Update any display or conversion logic
 *
 * Missing tokens won't cause compilation errors but will lead to
 * unexpected runtime behavior or parse failures!
 */

pub fn lexer<'t>() -> impl Parser<'t, &'t str, Vec<Spanned<Token<'t>>>, Extra<'t>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(|n| Token::Literal(Literal::Num(n)));

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
        .map(|c| Token::Literal(Literal::Char(c)));

    let string = just('"')
        .ignore_then(
            any()
                .filter(|c| *c != '\\' && *c != '"')
                .or(escape)
                .repeated()
                .to_slice(),
        )
        .then_ignore(just('"'))
        .map(|s| Token::Literal(Literal::Str(s)));

    // Lexer prioritizes the first defined token when ambiguous
    // therefore longer tokens must be defined before shorter ones

    // Multi-character operators
    let multi_op = choice((
        just("+=").to(OpT::ADD_ASSIGN),
        just("-=").to(OpT::SUB_ASSIGN),
        just("*=").to(OpT::MUL_ASSIGN),
        just("/=").to(OpT::DIV_ASSIGN),
        just("%=").to(OpT::REM_ASSIGN),
        just("<=").to(OpT::LESS_EQ),
        just(">=").to(OpT::GREATER_EQ),
        just("==").to(OpT::EQ),
        just("!=").to(OpT::NOT_EQ),
        // Keywords that are operators
        just("and").to(OpT::AND),
        just("or").to(OpT::OR),
        just("xor").to(OpT::XOR),
    ))
    .map(|op| op.0);

    // Single-character operators
    let single_op = choice((
        just('=').to(OpT::ASSIGN),
        just('+').to(OpT::ADD),
        just('-').to(OpT::SUB),
        just('*').to(OpT::MUL),
        just('/').to(OpT::DIV),
        just('%').to(OpT::REM),
        just('<').to(OpT::LESS),
        just('>').to(OpT::GREATER),
        just('!').to(OpT::NOT),
        just('&').to(OpT::MERGE),
    ))
    .map(|op| op.0);

    // Multi-character control tokens
    let multi_ctrl = choice((
        just("->").to(CtrlT::ARROW),
        just("=>").to(CtrlT::DOUBLE_ARROW),
    ))
    .map(|ctrl| ctrl.0);

    // Single-character control tokens
    let single_ctrl = choice((
        just('.').to(CtrlT::DOT),
        just(':').to(CtrlT::COLON),
        just(',').to(CtrlT::COMMA),
        just('~').to(CtrlT::TILDE),
        just('|').to(CtrlT::PIPE),
        just('\\').to(CtrlT::BACKSLASH),
        just('_').to(CtrlT::UNDERSCORE),
    ))
    .map(|ctrl| ctrl.0);

    // Delimiters group
    let open = choice((
        just('(').to(OpenT::PAREN),
        just('[').to(OpenT::BRACKET),
        just('{').to(OpenT::BRACE),
    ))
    .map(|open| open.0);

    let close = choice((
        just(')').to(CloseT::PAREN),
        just(']').to(CloseT::BRACKET),
        just('}').to(CloseT::BRACE),
    ))
    .map(|close| close.0);

    // TODO this looks kind of ugly, move keywords somewhere
    // Keywords and identifiers
    let word = text::ident().map(|ident| match ident {
        "type" => KwT::TYPE.0,
        "fn" => KwT::FN.0,
        "functor" => KwT::FUNCTOR.0,
        "let" => KwT::LET.0,
        "in" => KwT::IN.0,
        "if" => KwT::IF.0,
        "then" => KwT::THEN.0,
        "else" => KwT::ELSE.0,
        "case" => KwT::CASE.0,
        "of" => KwT::OF.0,
        "import" => KwT::IMPORT.0,
        "export" => KwT::EXPORT.0,
        "forall" => KwT::FORALL.0,
        "true" => Token::Literal(Literal::Bool(true)),
        "false" => Token::Literal(Literal::Bool(false)),
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
        open,
        close,
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
