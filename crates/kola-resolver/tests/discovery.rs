use std::sync::Arc;

use camino::Utf8PathBuf;
use kola_interner::StrInterner;
use kola_print::{bumpalo::Bump, PrintOptions};
use kola_resolver::prelude::resolve;
use kola_resolver::surface::Key;
use kola_resolver::symbol::AnySym;
use kola_span::{Report, SourceManager, io::MockFileSystem};
use kola_syntax::lexer::try_tokenize;
use kola_syntax::parser::{ParseInput, parse};
use kola_tree::node::{ModuleName, ValueName};
use kola_tree::source::TreeMap;

fn compile_source(text: &str) -> (kola_resolver::db::Db, StrInterner, Report) {
    let path = Utf8PathBuf::from("test.kl");
    let mut fs = MockFileSystem::new();
    fs.add_file(path.clone(), text.to_owned());

    let mut sources = SourceManager::new(Arc::new(fs));
    let mut interner = StrInterner::default();
    let mut report = Report::new();

    let output = try_tokenize(&mut sources, path.as_path()).unwrap();
    let source_id = output.root_id;
    let tokens = output.token_map[&source_id].clone();
    let output = parse(ParseInput::new(source_id, tokens, &mut interner), &mut report);

    let trees = output.tree.expect("parse produced no tree");
    assert!(report.is_empty(), "{report:?}");

    let mut tree_map = TreeMap::new();
    tree_map.insert(source_id, trees);

    let arena = Bump::new();
    let db = resolve(
        sources,
        tree_map,
        &arena,
        &mut interner,
        &mut report,
        PrintOptions::default(),
    )
    .unwrap();

    (db, interner, report)
}

fn resolve_source(text: &str) -> (kola_resolver::db::Db, StrInterner) {
    let (db, interner, report) = compile_source(text);
    assert!(report.is_empty(), "{report:?}");
    (db, interner)
}

#[test]
fn same_module_forward_reference_resolves() {
    let (db, mut interner) = resolve_source(
        "{
            a = b,
            b = 1,
            main = a,
        }",
    );

    let root = db.module(db.root);
    for name in ["a", "b", "main"] {
        let key = Key::Value(ValueName::new(interner.intern(name)));
        assert!(
            root.surface.definition(key).is_some(),
            "missing surface entry for {name}"
        );
    }
}

#[test]
fn unresolved_unqualified_value_reports_during_discovery() {
    let (_, _, report) = compile_source("{ a = missing }");
    assert!(!report.is_empty());
}

#[test]
fn abstract_value_and_module_are_obligations() {
    let (db, mut interner) = resolve_source(
        "{
            module inner = {
                abstract : Num,
                defined = 1,
            },
            main = 1,
        }",
    );

    let root = db.module(db.root);
    let inner_key = Key::Module(ModuleName::new(interner.intern("inner")));
    let inner_def = root
        .surface
        .definition(inner_key)
        .expect("missing inner module definition");
    let AnySym::Module(inner_sym) = inner_def.sym else {
        panic!("inner definition is not a module");
    };

    let inner = db.module(inner_sym);
    let abstract_key = Key::Value(ValueName::new(interner.intern("abstract")));
    let defined_key = Key::Value(ValueName::new(interner.intern("defined")));

    assert!(inner.surface.definition(defined_key).is_some());
    assert!(inner.surface.definition(abstract_key).is_none());
    assert!(inner.surface.obligation(abstract_key).is_some());
    assert_eq!(inner.surface.outstanding().count(), 1);
}
