use unscanny::Scanner;

use kola_span::{Diagnostic, Loc, Report, SourceId, Span};

use crate::token::{CommentT, LiteralT, Token, Tokens};

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

    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    lex(source, text, &mut tokens, &mut errors);

    report.extend_diagnostics(errors);

    if tokens.is_empty() && !report.is_empty() {
        None
    } else {
        Some(tokens)
    }
}

pub fn try_tokenize(input: LexInput<'_>) -> Result<Tokens<'_>, Vec<Diagnostic>> {
    let LexInput { source, text } = input;

    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    lex(source, text, &mut tokens, &mut errors);

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
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

fn lex<'t>(source: SourceId, text: &'t str, tokens: &mut Tokens<'t>, errors: &mut Vec<Diagnostic>) {
    let mut s = Scanner::new(text);

    while !s.done() {
        s.eat_whitespace();
        if s.done() {
            break;
        }

        let start = s.cursor();

        match s.peek() {
            Some('#') => lex_comment(&mut s, source, tokens),
            Some('"') => lex_string(&mut s, source, text, tokens, errors),
            Some(c) if c.is_ascii_digit() => lex_number(&mut s, source, text, tokens),
            Some(c) if is_ident_start(c) => lex_word(&mut s, source, text, tokens),
            Some(_) => lex_punct(&mut s, source, tokens, errors),
            None => break,
        }

        // Safety valve: if we didn't advance, skip one character to avoid infinite loops
        if s.cursor() == start {
            let c = s.eat().unwrap();
            let loc = Loc::new(source, Span::new(start, s.cursor()));
            errors.push(Diagnostic::error(
                loc,
                format!("unexpected character '{c}'"),
            ));
        }
    }
}

fn lex_comment<'t>(s: &mut Scanner<'t>, source: SourceId, tokens: &mut Tokens<'t>) {
    let start = s.cursor();

    // Eat the first '#'
    s.eat();

    // Check for doc comment (#|)
    let is_doc = s.eat_if('|');

    // Eat until end of line
    s.eat_until('\n');
    let end = s.cursor();

    // The content starts after '#' or '#|'
    let content_start = if is_doc { start + 2 } else { start + 1 };
    let content = &s.string()[content_start..end];

    let loc = Loc::new(source, Span::new(start, end));
    let comment = if is_doc {
        CommentT::Doc(content)
    } else {
        CommentT::Line(content)
    };
    tokens.push((Token::Comment(comment), loc));
}

fn lex_string<'t>(
    s: &mut Scanner<'t>,
    source: SourceId,
    text: &'t str,
    tokens: &mut Tokens<'t>,
    errors: &mut Vec<Diagnostic>,
) {
    let start = s.cursor();

    // Eat the opening quote
    s.eat();

    loop {
        match s.peek() {
            Some('"') => {
                s.eat();
                break;
            }
            Some('\\') => {
                // Skip escape sequence (backslash + next char)
                s.eat();
                s.eat();
            }
            Some(_) => {
                s.eat();
            }
            None => {
                let loc = Loc::new(source, Span::new(start, s.cursor()));
                errors.push(Diagnostic::error(loc, "unterminated string literal"));
                return;
            }
        }
    }

    let end = s.cursor();
    // Inner content without quotes
    let inner = &text[start + 1..end - 1];
    let loc = Loc::new(source, Span::new(start, end));
    tokens.push((Token::Literal(LiteralT::Str(inner)), loc));
}

fn lex_number<'t>(s: &mut Scanner<'t>, source: SourceId, text: &'t str, tokens: &mut Tokens<'t>) {
    let start = s.cursor();

    s.eat_while(|c: char| c.is_ascii_digit());

    // Check for decimal part
    if s.peek() == Some('.') {
        // Only consume the dot if followed by a digit (avoid `42.method`)
        if s.scout(1).is_some_and(|c| c.is_ascii_digit()) {
            s.eat(); // the '.'
            s.eat_while(|c: char| c.is_ascii_digit());
        }
    }

    let end = s.cursor();
    let slice = &text[start..end];
    let n: f64 = slice.parse().unwrap();
    let loc = Loc::new(source, Span::new(start, end));
    tokens.push((Token::Literal(LiteralT::Num(n)), loc));
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn lex_word<'t>(s: &mut Scanner<'t>, source: SourceId, text: &'t str, tokens: &mut Tokens<'t>) {
    let start = s.cursor();
    s.eat_while(is_ident_continue);
    let end = s.cursor();
    let word = &text[start..end];
    let loc = Loc::new(source, Span::new(start, end));

    let token = match word {
        // Keywords
        "None" => Token::Atom("None"),
        "module" => Token::Atom("module"),
        "import" => Token::Atom("import"),
        "export" => Token::Atom("export"),
        "functor" => Token::Atom("functor"),
        "effect" => Token::Atom("effect"),
        "type" => Token::Atom("type"),
        "forall" => Token::Atom("forall"),
        "fn" => Token::Atom("fn"),
        "do" => Token::Atom("do"),
        "let" => Token::Atom("let"),
        "in" => Token::Atom("in"),
        "if" => Token::Atom("if"),
        "then" => Token::Atom("then"),
        "else" => Token::Atom("else"),
        "case" => Token::Atom("case"),
        "handle" => Token::Atom("handle"),
        // Boolean literals
        "true" => Token::Literal(LiteralT::Bool(true)),
        "false" => Token::Literal(LiteralT::Bool(false)),
        // Lone underscore is an atom (wildcard pattern)
        "_" => Token::Atom("_"),
        // Identifiers — differentiate upper vs lower by first char
        _ => {
            let first = word.chars().next().unwrap();
            if first.is_ascii_uppercase() {
                Token::UpperSymbol(word)
            } else {
                Token::LowerSymbol(word)
            }
        }
    };

    tokens.push((token, loc));
}

fn lex_punct<'t>(
    s: &mut Scanner<'t>,
    source: SourceId,
    tokens: &mut Tokens<'t>,
    errors: &mut Vec<Diagnostic>,
) {
    let start = s.cursor();
    let c = s.eat().unwrap();

    // Try to extend into multi-character tokens
    let atom: Option<&'static str> = match c {
        '(' => {
            // Check for unit literal "()"
            if s.eat_if(')') {
                let loc = Loc::new(source, Span::new(start, s.cursor()));
                tokens.push((Token::Literal(LiteralT::Unit), loc));
                return;
            }
            Some("(")
        }
        ')' => Some(")"),
        '[' => Some("["),
        ']' => Some("]"),
        '{' => Some("{"),
        '}' => Some("}"),
        ',' => Some(","),
        '\'' => Some("'"),
        '~' => Some("~"),
        '@' => Some("@"),
        '\\' => Some("\\"),
        '_' => Some("_"),

        '+' => Some(if s.eat_if('=') {
            "+="
        } else if s.eat_if('+') {
            "++"
        } else {
            "+"
        }),

        '-' => Some(if s.eat_if('=') {
            "-="
        } else if s.eat_if('>') {
            "->"
        } else {
            "-"
        }),

        '*' => Some(if s.eat_if('=') { "*=" } else { "*" }),
        '/' => Some(if s.eat_if('=') { "/=" } else { "/" }),
        '%' => Some(if s.eat_if('=') { "%=" } else { "%" }),

        '=' => Some(if s.eat_if('=') {
            "=="
        } else if s.eat_if('>') {
            "=>"
        } else {
            "="
        }),

        '!' => Some(if s.eat_if('=') { "!=" } else { "!" }),
        '&' => Some(if s.eat_if('&') { "&&" } else { "&" }),

        '|' => Some(if s.eat_if('|') {
            "||"
        } else if s.eat_if('>') {
            "|>"
        } else {
            "|"
        }),

        '<' => Some(if s.eat_if('=') {
            "<="
        } else if s.eat_if('|') {
            "<|"
        } else {
            "<"
        }),

        '>' => Some(if s.eat_if('=') { ">=" } else { ">" }),

        ':' => Some(if s.eat_if(':') { "::" } else { ":" }),

        '.' => Some(if s.eat_if('.') {
            if s.eat_if('.') {
                "..."
            } else {
                // Two dots is not a valid token — emit the first dot,
                // let the second dot be picked up next iteration
                s.jump(start + 1);
                "."
            }
        } else {
            "."
        }),

        _ => {
            let loc = Loc::new(source, Span::new(start, s.cursor()));
            errors.push(Diagnostic::error(
                loc,
                format!("unexpected character '{c}'"),
            ));
            None
        }
    };

    if let Some(atom) = atom {
        let loc = Loc::new(source, Span::new(start, s.cursor()));
        tokens.push((Token::Atom(atom), loc));
    }
}

#[cfg(test)]
mod test {
    use camino::Utf8PathBuf;
    use kola_span::{Located, Report, Span};
    use kola_utils::interner::PathInterner;

    use super::tokenize;
    use crate::{
        lexer::LexInput,
        token::{CommentT, LiteralT, Token},
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
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens[0].0,
            Token::Comment(CommentT::Line(" This is a comment"))
        );
        assert_eq!(
            tokens[1].0,
            Token::Comment(CommentT::Line(" And another one"))
        );

        let tokens = tokenize_str("# Comment\nlet");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].0, Token::Comment(CommentT::Line(" Comment")));
        assert_eq!(tokens[1].0, Token::Atom("let"));
    }

    #[test]
    fn test_doc_comments() {
        let tokens = tokenize_str("#| This is a doc comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].0,
            Token::Comment(CommentT::Doc(" This is a doc comment"))
        );
    }

    #[test]
    fn test_lower_symbols() {
        let tokens = tokenize_str("x y z abc_123 camelCase snake_case");
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::LowerSymbol("x"));
        assert_eq!(tokens[1].0, Token::LowerSymbol("y"));
        assert_eq!(tokens[2].0, Token::LowerSymbol("z"));
        assert_eq!(tokens[3].0, Token::LowerSymbol("abc_123"));
        assert_eq!(tokens[4].0, Token::LowerSymbol("camelCase"));
        assert_eq!(tokens[5].0, Token::LowerSymbol("snake_case"));
    }

    #[test]
    fn test_upper_symbols() {
        let tokens = tokenize_str("Option Some Empty MyType");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].0, Token::UpperSymbol("Option"));
        assert_eq!(tokens[1].0, Token::UpperSymbol("Some"));
        assert_eq!(tokens[2].0, Token::UpperSymbol("Empty"));
        assert_eq!(tokens[3].0, Token::UpperSymbol("MyType"));
    }

    #[test]
    fn test_keywords() {
        let input = "module import export functor type forall fn let in if then else case handle effect do None";
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
        assert_eq!(tokens[13].0, Token::Atom("handle"));
        assert_eq!(tokens[14].0, Token::Atom("effect"));
        assert_eq!(tokens[15].0, Token::Atom("do"));
        assert_eq!(tokens[16].0, Token::Atom("None"));
    }

    #[test]
    fn test_keyword_prefixed_idents() {
        let tokens = tokenize_str("letter inform donut types");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].0, Token::LowerSymbol("letter"));
        assert_eq!(tokens[1].0, Token::LowerSymbol("inform"));
        assert_eq!(tokens[2].0, Token::LowerSymbol("donut"));
        assert_eq!(tokens[3].0, Token::LowerSymbol("types"));
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
    fn test_unit_literal() {
        let tokens = tokenize_str("()");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, Token::Literal(LiteralT::Unit));
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
        let tokens = tokenize_str("+= -= *= /= %= <= >= == != -> => :: |> <| && || ++");
        assert_eq!(tokens.len(), 17);
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
        assert_eq!(tokens[12].0, Token::Atom("|>"));
        assert_eq!(tokens[13].0, Token::Atom("<|"));
        assert_eq!(tokens[14].0, Token::Atom("&&"));
        assert_eq!(tokens[15].0, Token::Atom("||"));
        assert_eq!(tokens[16].0, Token::Atom("++"));
    }

    #[test]
    fn test_triple_dot() {
        let tokens = tokenize_str("...");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, Token::Atom("..."));
    }

    #[test]
    fn test_complex_expressions() {
        let input = "let x = 42 in x + 10";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Atom("let"));
        assert_eq!(tokens[1].0, Token::LowerSymbol("x"));
        assert_eq!(tokens[2].0, Token::Atom("="));
        assert_eq!(tokens[3].0, Token::Literal(LiteralT::Num(42.0)));
        assert_eq!(tokens[4].0, Token::Atom("in"));
        assert_eq!(tokens[5].0, Token::LowerSymbol("x"));
        assert_eq!(tokens[6].0, Token::Atom("+"));
        assert_eq!(tokens[7].0, Token::Literal(LiteralT::Num(10.0)));

        let input = "fn x => x * 2";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Atom("fn"));
        assert_eq!(tokens[1].0, Token::LowerSymbol("x"));
        assert_eq!(tokens[2].0, Token::Atom("=>"));
        assert_eq!(tokens[3].0, Token::LowerSymbol("x"));
        assert_eq!(tokens[4].0, Token::Atom("*"));
        assert_eq!(tokens[5].0, Token::Literal(LiteralT::Num(2.0)));

        let input = "if a == b then \"equal\" else \"not equal\"";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Atom("if"));
        assert_eq!(tokens[1].0, Token::LowerSymbol("a"));
        assert_eq!(tokens[2].0, Token::Atom("=="));
        assert_eq!(tokens[3].0, Token::LowerSymbol("b"));
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
        assert_eq!(tokens[1].0, Token::LowerSymbol("a"));
        assert_eq!(tokens[2].0, Token::Atom("."));
        assert_eq!(tokens[3].0, Token::LowerSymbol("a"));
        assert_eq!(tokens[4].0, Token::Atom("->"));
        assert_eq!(tokens[5].0, Token::LowerSymbol("a"));

        let input = "type Option = forall a . < Some: a, Empty >";
        let tokens = tokenize_str(input);
        assert_eq!(tokens.len(), 13);
        assert_eq!(tokens[0].0, Token::Atom("type"));
        assert_eq!(tokens[1].0, Token::UpperSymbol("Option"));
        assert_eq!(tokens[2].0, Token::Atom("="));
        assert_eq!(tokens[3].0, Token::Atom("forall"));
        assert_eq!(tokens[4].0, Token::LowerSymbol("a"));
        assert_eq!(tokens[5].0, Token::Atom("."));
        assert_eq!(tokens[6].0, Token::Atom("<"));
        assert_eq!(tokens[7].0, Token::UpperSymbol("Some"));
        assert_eq!(tokens[8].0, Token::Atom(":"));
        assert_eq!(tokens[9].0, Token::LowerSymbol("a"));
        assert_eq!(tokens[10].0, Token::Atom(","));
        assert_eq!(tokens[11].0, Token::UpperSymbol("Empty"));
        assert_eq!(tokens[12].0, Token::Atom(">"));
    }

    #[test]
    fn test_recovery() {
        let mut interner = PathInterner::new();
        let source = interner.intern(Utf8PathBuf::from("test"));
        let mut report = Report::new();

        let input = LexInput {
            source,
            text: "let x = ` 42",
        };

        // Invalid tokens should be skipped and lexing should continue
        let tokens = tokenize(input, &mut report);
        assert!(report.diagnostics.len() == 1);
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
}
