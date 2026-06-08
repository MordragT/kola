use camino::Utf8Path;

use unscanny::Scanner;

use kola_span::{Diagnostic, Issue, Loc, Report, SourceId, SourceManager, Span};

use crate::token::{CommentT, LiteralT, Token, TokenMap, Tokens};

/// The file extension for source files.
pub const EXTENSION: &str = "kl";

pub struct TokenOutput<'t> {
    pub root_id: SourceId,
    pub token_map: TokenMap<'t>,
}

/// Lex an entire project starting from the root path.
///
/// Uses a worklist approach: starts with the root file, extracts imports,
/// and pushes imported files onto the worklist. Returns a map of SourceId → Tokens
/// for all discovered files.
///
/// Import resolution: if file `foo/main.kl` does `import bar`, the imported file
/// is expected at `foo/bar.kl`.
pub fn tokenize<'t>(
    source_manager: &'t mut SourceManager,
    root_path: &Utf8Path,
    report: &mut Report,
) -> TokenOutput<'t> {
    let root = source_manager
        .fetch(root_path)
        .expect("Failed to read root file");

    let mut token_map = TokenMap::new();
    let mut worklist = vec![root];

    while let Some((id, source)) = worklist.pop() {
        let parent_dir = source_manager
            .get_path(id)
            .parent()
            .unwrap_or(Utf8Path::new(""))
            .to_owned();
        let mut tokens = Vec::new();

        lex(
            id,
            source,
            &parent_dir,
            &mut tokens,
            report,
            &mut worklist,
            source_manager,
        );

        token_map.insert(id, tokens);
    }

    TokenOutput {
        root_id: root.0,
        token_map,
    }
}

/// Like `tokenize` but returns a Result instead of using a Report.
pub fn try_tokenize<'t>(
    source_manager: &'t mut SourceManager,
    root_path: &Utf8Path,
) -> Result<TokenOutput<'t>, Report> {
    let root = match source_manager.fetch(root_path) {
        Ok(root) => root,
        Err(_) => {
            let mut report = Report::new();
            report.add_issue(Issue::error(format!("Failed to read file: {root_path}"), 0));
            return Err(report);
        }
    };

    let mut token_map = TokenMap::new();
    let mut report = Report::new();
    let mut worklist = vec![root];

    while let Some((id, source)) = worklist.pop() {
        let parent_dir = source_manager
            .get_path(id)
            .parent()
            .unwrap_or(Utf8Path::new(""))
            .to_owned();
        let mut tokens = Vec::new();

        lex(
            id,
            source,
            &parent_dir,
            &mut tokens,
            &mut report,
            &mut worklist,
            source_manager,
        );

        token_map.insert(id, tokens);
    }

    if report.is_empty() {
        Ok(TokenOutput {
            root_id: root.0,
            token_map,
        })
    } else {
        Err(report)
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

type Worklist = Vec<(SourceId, &'static str)>;

fn lex<'t>(
    source: SourceId,
    text: &'t str,
    parent_dir: &Utf8Path,
    tokens: &mut Tokens<'t>,
    report: &mut Report,
    worklist: &mut Worklist,
    source_manager: &mut SourceManager,
) {
    let mut s = Scanner::new(text);

    while !s.done() {
        s.eat_whitespace();
        if s.done() {
            break;
        }

        let start = s.cursor();

        match s.peek() {
            Some('#') => lex_comment(&mut s, source, tokens),
            Some('"') => lex_string(&mut s, source, text, tokens, report),
            Some(c) if c.is_ascii_digit() => lex_number(&mut s, source, text, tokens),
            Some(c) if is_ident_start(c) => lex_word(
                &mut s,
                source,
                text,
                tokens,
                report,
                parent_dir,
                worklist,
                source_manager,
            ),
            Some(_) => lex_punct(&mut s, source, tokens, report),
            None => break,
        }

        // Safety valve: if we didn't advance, skip one character to avoid infinite loops
        if s.cursor() == start {
            let c = s.eat().unwrap();
            let loc = Loc::new(source, Span::new(start, s.cursor()));
            report.add_diagnostic(Diagnostic::error(
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
    report: &mut Report,
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
                report.add_diagnostic(Diagnostic::error(loc, "unterminated string literal"));
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

fn lex_word<'t>(
    s: &mut Scanner<'t>,
    source: SourceId,
    text: &'t str,
    tokens: &mut Tokens<'t>,
    report: &mut Report,
    parent_dir: &Utf8Path,
    worklist: &mut Worklist,
    source_manager: &mut SourceManager,
) {
    let start = s.cursor();
    s.eat_while(is_ident_continue);
    let end = s.cursor();
    let word = &text[start..end];
    let loc = Loc::new(source, Span::new(start, end));

    let token = match word {
        // Keywords
        "None" => Token::Atom("None"),
        "module" => Token::Atom("module"),
        "import" => {
            // Import as lexer keyword: extract the module name that follows
            // and push the resolved child file path onto the worklist.
            s.eat_whitespace();
            let name_start = s.cursor();
            let id = if let Some(c) = s.peek() {
                if is_ident_start(c) {
                    s.eat_while(is_ident_continue);
                    let name = &text[name_start..s.cursor()];

                    let child = parent_dir.join(name).with_extension(EXTENSION);

                    let Ok(child) = source_manager.fetch(child.as_path()) else {
                        let loc2 = Loc::new(source, Span::new(name_start, s.cursor()));
                        report.add_diagnostic(Diagnostic::error(
                            loc2,
                            format!("failed to resolve import '{name}'"),
                        ));
                        return;
                    };
                    worklist.push(child);
                    child.0
                } else {
                    let loc2 = Loc::new(source, Span::new(name_start, name_start + 1));
                    report.add_diagnostic(Diagnostic::error(
                        loc2,
                        "expected module name after 'import'",
                    ));
                    return;
                }
            } else {
                let loc2 = Loc::new(source, Span::new(end, end));
                report.add_diagnostic(Diagnostic::error(
                    loc2,
                    "expected module name after 'import'",
                ));
                return;
            };
            Token::Import(id)
        }
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
    report: &mut Report,
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
            report.add_diagnostic(Diagnostic::error(
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

    use std::sync::Arc;

    use camino::Utf8Path;
    use kola_span::{Report, SourceManager, Span};
    use kola_utils::io::MockFileSystem;

    use super::lex;
    use crate::token::{CommentT, LiteralT, Token, Tokens};

    fn tokenize<'t>(text: &'t str) -> (Tokens<'t>, Report) {
        let path = Utf8Path::new("test.kl");
        let mut fs = MockFileSystem::new();
        fs.add_file(path.to_owned(), text.to_string());

        let mut source_manager = SourceManager::new(Arc::new(fs));
        let (source, text) = source_manager.fetch(path).unwrap();

        let mut tokens = Vec::new();
        let mut report = Report::new();
        lex(
            source,
            text,
            Utf8Path::new("."),
            &mut tokens,
            &mut report,
            &mut Vec::new(),
            &mut source_manager,
        );
        (tokens, report)
    }

    fn tokenize_str<'t>(text: &'t str) -> Tokens<'t> {
        let (tokens, errors) = tokenize(text);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);

        tokens
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
        // Invalid tokens should be skipped and lexing should continue
        let (tokens, errors) = tokenize("let x = ` 42");
        assert_eq!(errors.diagnostics.len(), 1);
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
