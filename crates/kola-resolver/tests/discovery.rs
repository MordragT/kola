use std::sync::Arc;

use camino::Utf8PathBuf;
use kola_interner::StrInterner;
use kola_print::{PrintOptions, bumpalo::Bump};
use kola_resolver::prelude::resolve;
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
    let output = parse(
        ParseInput::new(source_id, tokens, &mut interner),
        &mut report,
    );

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
        let key = ValueName::new(interner.intern(name));
        assert!(
            root.members.definitions.values.definition(key).is_some(),
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
    let inner_key = ModuleName::new(interner.intern("inner"));
    let inner_def = root
        .members
        .definitions
        .modules
        .definition(inner_key)
        .expect("missing inner module definition");
    let inner_sym = inner_def.sym;

    let inner = db.module(inner_sym);
    let abstract_key = ValueName::new(interner.intern("abstract"));
    let defined_key = ValueName::new(interner.intern("defined"));

    assert!(
        inner
            .members
            .definitions
            .values
            .definition(defined_key)
            .is_some()
    );
    assert!(
        inner
            .members
            .definitions
            .values
            .definition(abstract_key)
            .is_none()
    );
    assert!(
        inner
            .members
            .obligations
            .values
            .obligation(abstract_key)
            .is_some()
    );
    assert_eq!(inner.members.outstanding_values().count(), 1);
}

#[test]
fn using_supplies_only_outstanding_members() {
    let (db, mut interner) = resolve_source(
        "{
            module service = {
                port : Num,
                url = \"http://localhost\",
            },
            module configured = service using { port = 8080, host = \"localhost\" },
            main = 1,
        }",
    );

    let root = db.module(db.root);
    let configured_key = ModuleName::new(interner.intern("configured"));
    let configured_sym = root
        .members
        .definitions
        .modules
        .definition(configured_key)
        .expect("missing configured module")
        .sym;

    let configured = db.module(configured_sym);
    let port = ValueName::new(interner.intern("port"));
    let url = ValueName::new(interner.intern("url"));
    let host = ValueName::new(interner.intern("host"));

    assert!(
        configured
            .members
            .definitions
            .values
            .definition(port)
            .is_some()
    );
    assert!(
        configured
            .members
            .definitions
            .values
            .definition(url)
            .is_some()
    );
    assert!(
        configured
            .members
            .definitions
            .values
            .definition(host)
            .is_none()
    );
    assert!(
        configured
            .members
            .obligations
            .values
            .obligation(port)
            .is_some()
    );
    assert_eq!(configured.members.outstanding_values().count(), 0);
}

#[test]
fn over_prefers_left_and_unions_both_sides() {
    let (db, mut interner) = resolve_source(
        "{
            module a = { x = 1, y = 2 },
            module b = { x = 3, z = 4 },
            module c = a over b,
            main = 1,
        }",
    );

    let root = db.module(db.root);
    let c_key = ModuleName::new(interner.intern("c"));
    let c_sym = root
        .members
        .definitions
        .modules
        .definition(c_key)
        .expect("missing composed module")
        .sym;
    let c = db.module(c_sym);

    for name in ["x", "y", "z"] {
        let key = ValueName::new(interner.intern(name));
        assert!(
            c.members.definitions.values.definition(key).is_some(),
            "missing {name} in composed module"
        );
    }

    assert_eq!(c.members.outstanding_values().count(), 0);
}

#[test]
fn with_unions_compatible_members() {
    let (db, mut interner) = resolve_source(
        "{
            module a = { x = 1 },
            module b = { y = 2 },
            module c = a with b,
            main = 1,
        }",
    );

    let root = db.module(db.root);
    let c_key = ModuleName::new(interner.intern("c"));
    let c_sym = root
        .members
        .definitions
        .modules
        .definition(c_key)
        .expect("missing composed module")
        .sym;
    let c = db.module(c_sym);

    for name in ["x", "y"] {
        let key = ValueName::new(interner.intern(name));
        assert!(
            c.members.definitions.values.definition(key).is_some(),
            "missing {name} in composed module"
        );
    }
}

#[test]
fn with_rejects_competing_definitions() {
    let (_, _, report) = compile_source(
        "{
            module a = { x = 1 },
            module b = { x = 2 },
            module c = a with b,
            main = 1,
        }",
    );

    assert!(!report.is_empty());
}
