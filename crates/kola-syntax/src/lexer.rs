use chumsky::prelude::*;

use crate::{
    error::{SyntaxError, SyntaxErrors},
    span::{Span, Spanned},
    token::{Literal, Token, Tokens},
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
 * are properly handled in the lexer.
 *
 * When adding new tokens or modifying existing ones:
 * - Update the Token newtype wrapers in token.rs
 * - Update the lexer patterns here
 * - Update the parser to handle the new token
 *
 * Missing tokens won't cause compilation errors but will lead to
 * unexpected runtime behavior or parse failures!
 */

pub fn lexer<'t>() -> impl Parser<'t, &'t str, Vec<Spanned<Token<'t>>>, Extra<'t>> {
    // Define a common escape sequence parser
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

    // Combined literal parser
    let literal = choice((
        // Number literals
        text::int(10)
            .then(just('.').then(text::digits(10)).or_not())
            .to_slice()
            .from_str()
            .unwrapped()
            .map(Literal::Num),
        // Character literals
        just('\'')
            .ignore_then(any().filter(|c| *c != '\\' && *c != '\'').or(escape))
            .then_ignore(just('\''))
            .map(Literal::Char),
        // String literals
        just('"')
            .ignore_then(
                any()
                    .filter(|c| *c != '\\' && *c != '"')
                    .or(escape)
                    .repeated()
                    .to_slice(),
            )
            .then_ignore(just('"'))
            .map(Literal::Str),
        // Boolean literals
        just("true").to(Literal::Bool(true)),
        just("false").to(Literal::Bool(false)),
    ))
    .map(Token::Literal)
    .boxed();

    // Single-character tokens
    let punct = choice((
        // Operators
        just('=').to(Token::Atom("=")),
        just('+').to(Token::Atom("+")),
        just('-').to(Token::Atom("-")),
        just('*').to(Token::Atom("*")),
        just('/').to(Token::Atom("/")),
        just('%').to(Token::Atom("%")),
        just('!').to(Token::Atom("!")),
        just('&').to(Token::Atom("&")),
        // Control tokens
        just('.').to(Token::Atom(".")),
        just(':').to(Token::Atom(":")),
        just(',').to(Token::Atom(",")),
        just('~').to(Token::Atom("~")),
        just('|').to(Token::Atom("|")),
        just('\\').to(Token::Atom("\\")),
        just('_').to(Token::Atom("_")),
        // Delimiters
        just('(').to(Token::Atom("(")),
        just('[').to(Token::Atom("[")),
        just('{').to(Token::Atom("{")),
        just(')').to(Token::Atom(")")),
        just(']').to(Token::Atom("]")),
        just('}').to(Token::Atom("}")),
        // Shared
        just('<').to(Token::Atom("<")),
        just('>').to(Token::Atom(">")),
    ))
    .boxed();

    // Two-character tokens
    let joint = choice((
        just("+=").to(Token::Atom("+=")),
        just("-=").to(Token::Atom("-=")),
        just("*=").to(Token::Atom("*=")),
        just("/=").to(Token::Atom("/=")),
        just("%=").to(Token::Atom("%=")),
        just("<=").to(Token::Atom("<=")),
        just(">=").to(Token::Atom(">=")),
        just("==").to(Token::Atom("==")),
        just("!=").to(Token::Atom("!=")),
        just("->").to(Token::Atom("->")),
        just("=>").to(Token::Atom("=>")),
    ))
    .boxed();

    // Multi-character tokens
    let word = choice((
        // Type and module keywords
        just("module").to(Token::Atom("module")),
        just("import").to(Token::Atom("import")),
        just("export").to(Token::Atom("export")),
        just("functor").to(Token::Atom("functor")),
        just("type").to(Token::Atom("type")),
        just("forall").to(Token::Atom("forall")),
        // Expression keywords
        just("fn").to(Token::Atom("fn")),
        just("let").to(Token::Atom("let")),
        just("in").to(Token::Atom("in")),
        just("if").to(Token::Atom("if")),
        just("then").to(Token::Atom("then")),
        just("else").to(Token::Atom("else")),
        just("case").to(Token::Atom("case")),
        just("of").to(Token::Atom("of")),
        // Logical operators
        just("and").to(Token::Atom("and")),
        just("or").to(Token::Atom("or")),
        just("xor").to(Token::Atom("xor")),
    ))
    .boxed();

    let symbol = text::ident().map(Token::Symbol);

    let token = choice((literal, word, joint, punct, symbol)).boxed();

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
