use chumsky::{input::StrInput, prelude::*};

use kola_span::{Diagnostic, Loc, Report, SourceId};

use crate::token::{LiteralT, Token, Tokens};

pub type Error<'t> = Rich<'t, char, Loc>;
pub type Extra<'t> = extra::Err<Error<'t>>;

// pub struct LexerInput<'t> {
//     pub key: PathKey,
//     pub cache: &'t SourceCache,
// }

// impl<'t> Input<'t> for LexerInput<'t> {
//     type Span = Loc;
//     type Token = char;
//     type MaybeToken = char;
//     type Cursor = usize;
//     type Cache = (&'t str, PathKey);

//     fn begin(self) -> (Self::Cursor, Self::Cache) {
//         let Self { key, cache } = self;
//         (0, (cache[key].text(), key))
//     }

//     fn cursor_location(cursor: &Self::Cursor) -> usize {
//         *cursor
//     }

//     unsafe fn next_maybe(
//         cache: &mut Self::Cache,
//         cursor: &mut Self::Cursor,
//     ) -> Option<Self::MaybeToken> {
//         unsafe { <&'t str as Input>::next_maybe(&mut cache.0, cursor) }
//     }

//     unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
//         Loc::from_range(cache.1, *range.start..*range.end)
//     }
// }

// impl<'t> ValueInput<'t> for LexerInput<'t> {
//     unsafe fn next(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::Token> {
//         unsafe { Self::next_maybe(cache, cursor) }
//     }
// }

// impl<'t> ExactSizeInput<'t> for LexerInput<'t> {
//     unsafe fn span_from(cache: &mut Self::Cache, range: RangeFrom<&Self::Cursor>) -> Self::Span {
//         Loc::from_range(cache.1, *range.start..cache.0.len())
//     }
// }

// impl<'t> SliceInput<'t> for LexerInput<'t> {
//     type Slice = &'t str;

//     fn full_slice(cache: &mut Self::Cache) -> Self::Slice {
//         cache.0
//     }

//     unsafe fn slice(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Slice {
//         unsafe { <&'t str as SliceInput>::slice(&mut cache.0, range) }
//     }

//     unsafe fn slice_from(cache: &mut Self::Cache, from: RangeFrom<&Self::Cursor>) -> Self::Slice {
//         unsafe { <&'t str as SliceInput>::slice_from(&mut cache.0, from) }
//     }
// }

pub struct LexInput<'t> {
    pub source: SourceId,
    pub text: &'t str,
}

impl<'t> LexInput<'t> {
    pub fn new(source: SourceId, text: &'t str) -> Self {
        Self { source, text }
    }
}

pub fn tokenize<'t>(input: LexInput<'t>, report: &mut Report) -> Option<Tokens<'t>> {
    let LexInput { source, text } = input;

    let input = text.with_context::<Loc>(source);

    let lexer = lexer();
    let (tokens, errors) = lexer.parse(input).into_output_errors();
    report.extend_diagnostics(errors.into_iter().map(Diagnostic::from));

    tokens
}

pub fn try_tokenize(input: LexInput<'_>) -> Result<Tokens<'_>, Vec<Error<'_>>> {
    let LexInput { source, text } = input;

    let input = text.with_context::<Loc>(source);

    let lexer = lexer();
    lexer.parse(input).into_result()
}

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

pub fn lexer<'t, I>() -> impl Parser<'t, I, Tokens<'t>, Extra<'t>>
where
    I: StrInput<'t, Token = char, Slice = &'t str, Span = Loc>,
{
    // Define a common escape sequence parser
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('\''))
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
            .map(LiteralT::Num),
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
            .map(LiteralT::Str),
        // Boolean literals
        just("true").to(LiteralT::Bool(true)),
        just("false").to(LiteralT::Bool(false)),
        just("()").to(LiteralT::Unit),
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
        just('\'').to(Token::Atom("'")),
        just('~').to(Token::Atom("~")),
        just('@').to(Token::Atom("@")),
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
        just("...").to(Token::Atom("...")),
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
        just("::").to(Token::Atom("::")),
        just("|>").to(Token::Atom("|>")),
        just("<|").to(Token::Atom("<|")),
        just("&&").to(Token::Atom("&&")),
        just("||").to(Token::Atom("||")),
        just("++").to(Token::Atom("++")),
    ))
    .boxed();

    // Combined word and symbol handling
    let ident = text::ident()
        .map(|s| match s {
            "None" => Token::Atom("None"),
            // Type and module keywords
            "module" => Token::Atom("module"),
            "import" => Token::Atom("import"),
            "export" => Token::Atom("export"),
            "effect" => Token::Atom("effect"),
            "functor" => Token::Atom("functor"),
            "type" => Token::Atom("type"),
            "forall" => Token::Atom("forall"),
            // Expression keywords
            "fn" => Token::Atom("fn"),
            "do" => Token::Atom("do"),
            "let" => Token::Atom("let"),
            "in" => Token::Atom("in"),
            "if" => Token::Atom("if"),
            "then" => Token::Atom("then"),
            "else" => Token::Atom("else"),
            "case" => Token::Atom("case"),
            "handle" => Token::Atom("handle"),
            // If not a keyword, treat as a symbol
            _ => Token::Symbol(s),
        })
        .boxed();

    let token = choice((literal, joint, punct, ident)).boxed();

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

#[cfg(test)]
mod test {
    use camino::Utf8PathBuf;
    use kola_span::{Located, Report, Span};
    use kola_utils::interner::PathInterner;

    use super::tokenize;
    use crate::{
        lexer::LexInput,
        token::{LiteralT, Token},
    };

    fn tokenize_str(text: &str) -> Vec<Located<Token<'_>>> {
        let mut interner = PathInterner::new();
        let source = interner.intern(Utf8PathBuf::from("test"));

        let input = LexInput { source, text };

        let mut report = Report::new();

        let tokens = tokenize(input, &mut report);
        assert!(report.is_empty(), "Unexpected errors: {:?}", report);
        tokens.expect("Tokenization failed")
    }

    #[test]
    fn test_empty() {
        let tokens = tokenize_str("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_whitespace() {
        let tokens = tokenize_str("   \n\t   ");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_comments() {
        let tokens = tokenize_str("# This is a comment\n# And another one");
        assert!(tokens.is_empty());

        let tokens = tokenize_str("# Comment\nlet");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, Token::Atom("let"));
    }

    #[test]
    fn test_symbols() {
        let tokens = tokenize_str("x y z abc_123 camelCase snake_case");
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Symbol("x"));
        assert_eq!(tokens[1].0, Token::Symbol("y"));
        assert_eq!(tokens[2].0, Token::Symbol("z"));
        assert_eq!(tokens[3].0, Token::Symbol("abc_123"));
        assert_eq!(tokens[4].0, Token::Symbol("camelCase"));
        assert_eq!(tokens[5].0, Token::Symbol("snake_case"));
    }

    #[test]
    fn test_keywords() {
        let input =
            "module import export functor type forall fn let in if then else case of and or xor";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 17);
        assert_eq!(tokens[0].0, Token::Atom("module"));
        assert_eq!(tokens[1].0, Token::Atom("import"));
        assert_eq!(tokens[2].0, Token::Atom("export"));
        assert_eq!(tokens[3].0, Token::Atom("functor"));
        assert_eq!(tokens[4].0, Token::Atom("type"));
        assert_eq!(tokens[5].0, Token::Atom("forall"));
        assert_eq!(tokens[6].0, Token::Atom("fn"));
        assert_eq!(tokens[7].0, Token::Atom("let"));
        assert_eq!(tokens[8].0, Token::Atom("in"));
        assert_eq!(tokens[9].0, Token::Atom("if"));
        assert_eq!(tokens[10].0, Token::Atom("then"));
        assert_eq!(tokens[11].0, Token::Atom("else"));
        assert_eq!(tokens[12].0, Token::Atom("case"));
        assert_eq!(tokens[13].0, Token::Atom("of"));
        assert_eq!(tokens[14].0, Token::Atom("and"));
        assert_eq!(tokens[15].0, Token::Atom("or"));
        assert_eq!(tokens[16].0, Token::Atom("xor"));
    }

    #[test]
    fn test_numeric_literals() {
        let tokens = tokenize_str("123 45.67 0 0.0");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].0, Token::Literal(LiteralT::Num(123.0)));
        assert_eq!(tokens[1].0, Token::Literal(LiteralT::Num(45.67)));
        assert_eq!(tokens[2].0, Token::Literal(LiteralT::Num(0.0)));
        assert_eq!(tokens[3].0, Token::Literal(LiteralT::Num(0.0)));
    }

    #[test]
    fn test_boolean_literals() {
        let tokens = tokenize_str("true false");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].0, Token::Literal(LiteralT::Bool(true)));
        assert_eq!(tokens[1].0, Token::Literal(LiteralT::Bool(false)));
    }

    #[test]
    fn test_string_literals() {
        let tokens = tokenize_str(r#""hello" "world" "\"quoted\"" "\n\t""#);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].0, Token::Literal(LiteralT::Str("hello")));
        assert_eq!(tokens[1].0, Token::Literal(LiteralT::Str("world")));
        assert_eq!(tokens[2].0, Token::Literal(LiteralT::Str("\\\"quoted\\\"")));
        assert_eq!(tokens[3].0, Token::Literal(LiteralT::Str("\\n\\t")));
    }

    #[test]
    fn test_single_char_operators() {
        let tokens = tokenize_str("= + - * / % ! & . : , ~ | \\ _");
        assert_eq!(tokens.len(), 15);
        assert_eq!(tokens[0].0, Token::Atom("="));
        assert_eq!(tokens[1].0, Token::Atom("+"));
        assert_eq!(tokens[2].0, Token::Atom("-"));
        assert_eq!(tokens[3].0, Token::Atom("*"));
        assert_eq!(tokens[4].0, Token::Atom("/"));
        assert_eq!(tokens[5].0, Token::Atom("%"));
        assert_eq!(tokens[6].0, Token::Atom("!"));
        assert_eq!(tokens[7].0, Token::Atom("&"));
        assert_eq!(tokens[8].0, Token::Atom("."));
        assert_eq!(tokens[9].0, Token::Atom(":"));
        assert_eq!(tokens[10].0, Token::Atom(","));
        assert_eq!(tokens[11].0, Token::Atom("~"));
        assert_eq!(tokens[12].0, Token::Atom("|"));
        assert_eq!(tokens[13].0, Token::Atom("\\"));
        assert_eq!(tokens[14].0, Token::Atom("_"));
    }

    #[test]
    fn test_delimiters() {
        let tokens = tokenize_str("( [ { ) ] } < >");
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Atom("("));
        assert_eq!(tokens[1].0, Token::Atom("["));
        assert_eq!(tokens[2].0, Token::Atom("{"));
        assert_eq!(tokens[3].0, Token::Atom(")"));
        assert_eq!(tokens[4].0, Token::Atom("]"));
        assert_eq!(tokens[5].0, Token::Atom("}"));
        assert_eq!(tokens[6].0, Token::Atom("<"));
        assert_eq!(tokens[7].0, Token::Atom(">"));
    }

    #[test]
    fn test_two_char_operators() {
        let tokens = tokenize_str("+= -= *= /= %= <= >= == != -> => ::");
        assert_eq!(tokens.len(), 12);
        assert_eq!(tokens[0].0, Token::Atom("+="));
        assert_eq!(tokens[1].0, Token::Atom("-="));
        assert_eq!(tokens[2].0, Token::Atom("*="));
        assert_eq!(tokens[3].0, Token::Atom("/="));
        assert_eq!(tokens[4].0, Token::Atom("%="));
        assert_eq!(tokens[5].0, Token::Atom("<="));
        assert_eq!(tokens[6].0, Token::Atom(">="));
        assert_eq!(tokens[7].0, Token::Atom("=="));
        assert_eq!(tokens[8].0, Token::Atom("!="));
        assert_eq!(tokens[9].0, Token::Atom("->"));
        assert_eq!(tokens[10].0, Token::Atom("=>"));
        assert_eq!(tokens[11].0, Token::Atom("::"));
    }

    #[test]
    fn test_complex_expressions() {
        let input = "let x = 42 in x + 10";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Atom("let"));
        assert_eq!(tokens[1].0, Token::Symbol("x"));
        assert_eq!(tokens[2].0, Token::Atom("="));
        assert_eq!(tokens[3].0, Token::Literal(LiteralT::Num(42.0)));
        assert_eq!(tokens[4].0, Token::Atom("in"));
        assert_eq!(tokens[5].0, Token::Symbol("x"));
        assert_eq!(tokens[6].0, Token::Atom("+"));
        assert_eq!(tokens[7].0, Token::Literal(LiteralT::Num(10.0)));

        let input = "fn x => x * 2";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Atom("fn"));
        assert_eq!(tokens[1].0, Token::Symbol("x"));
        assert_eq!(tokens[2].0, Token::Atom("=>"));
        assert_eq!(tokens[3].0, Token::Symbol("x"));
        assert_eq!(tokens[4].0, Token::Atom("*"));
        assert_eq!(tokens[5].0, Token::Literal(LiteralT::Num(2.0)));

        let input = "if a == b then \"equal\" else \"not equal\"";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Atom("if"));
        assert_eq!(tokens[1].0, Token::Symbol("a"));
        assert_eq!(tokens[2].0, Token::Atom("=="));
        assert_eq!(tokens[3].0, Token::Symbol("b"));
        assert_eq!(tokens[4].0, Token::Atom("then"));
        assert_eq!(tokens[5].0, Token::Literal(LiteralT::Str("equal")));
        assert_eq!(tokens[6].0, Token::Atom("else"));
        assert_eq!(tokens[7].0, Token::Literal(LiteralT::Str("not equal")));
    }

    #[test]
    fn test_complex_types() {
        let input = "forall a . a -> a";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Atom("forall"));
        assert_eq!(tokens[1].0, Token::Symbol("a"));
        assert_eq!(tokens[2].0, Token::Atom("."));
        assert_eq!(tokens[3].0, Token::Symbol("a"));
        assert_eq!(tokens[4].0, Token::Atom("->"));
        assert_eq!(tokens[5].0, Token::Symbol("a"));

        let input = "type Option = forall a . < Some: a, None >";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 13);
        assert_eq!(tokens[0].0, Token::Atom("type"));
        assert_eq!(tokens[1].0, Token::Symbol("Option"));
        assert_eq!(tokens[2].0, Token::Atom("="));
        assert_eq!(tokens[3].0, Token::Atom("forall"));
        assert_eq!(tokens[4].0, Token::Symbol("a"));
        assert_eq!(tokens[5].0, Token::Atom("."));
        assert_eq!(tokens[6].0, Token::Atom("<"));
        assert_eq!(tokens[7].0, Token::Symbol("Some"));
        assert_eq!(tokens[8].0, Token::Atom(":"));
        assert_eq!(tokens[9].0, Token::Symbol("a"));
        assert_eq!(tokens[10].0, Token::Atom(","));
        assert_eq!(tokens[11].0, Token::Symbol("None"));
        assert_eq!(tokens[12].0, Token::Atom(">"));
    }

    #[test]
    fn test_recovery() {
        let mut interner = PathInterner::new();
        let source = interner.intern(Utf8PathBuf::from("test"));
        let mut report = Report::new();

        let input = LexInput {
            source,
            text: "let x = @ 42",
        };

        // Invalid tokens should be skipped and lexing should continue
        let tokens = tokenize(input, &mut report);
        assert!(report.diagnostics.len() == 1); // Should have one error for the @
        assert!(tokens.is_some());
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), 4); // let, x, =, 42
    }

    #[test]
    fn test_spans() {
        let input = "let x = 42";
        let tokens = tokenize_str(input);

        assert_eq!(tokens[0].1.span, Span::new(0, 3)); // "let"
        assert_eq!(tokens[1].1.span, Span::new(4, 5)); // "x"
        assert_eq!(tokens[2].1.span, Span::new(6, 7)); // "="
        assert_eq!(tokens[3].1.span, Span::new(8, 10)); // "42"
    }

    // #[test]
    // fn test_mixed_code() {
    //     let input = r#"
    //         module Stack = {
    //             type Stack = forall a . { items: [a] }

    //             # Create a new stack
    //             let new = { items = [] }

    //             # Push to stack
    //             let push = fn stack => fn x =>
    //                 { stack | items = [x, ...stack.items] }

    //             # Pop from stack - returns the value and new stack
    //             let pop = fn stack =>
    //                 if stack.items == [] then
    //                     None
    //                 else
    //                     Some({ value = stack.items[0],
    //                           stack = { stack | items = stack.items[1:] } })
    //         }
    //     "#;

    //     let tokens = tokenize_str(input);
    //     assert!(tokens.len() > 20, "Should have tokenized complex code");

    //     // Spot check a few tokens
    //     let has_module = tokens.iter().any(|(t, _)| *t == Token::Atom("module"));
    //     let has_stack = tokens.iter().any(|(t, _)| *t == Token::Symbol("Stack"));
    //     let has_type = tokens.iter().any(|(t, _)| *t == Token::Atom("type"));
    //     let has_some = tokens.iter().any(|(t, _)| *t == Token::Symbol("Some"));

    //     assert!(has_module && has_stack && has_type && has_some);
    // }
}
