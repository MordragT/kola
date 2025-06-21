use std::{ops::ControlFlow, rc::Rc};

use camino::Utf8PathBuf;
use kola_resolver::phase::ResolvedNodes;
use kola_span::{Loc, Located, Report, SourceId, Span};
use kola_syntax::loc::Locations;
use kola_tree::prelude::*;
use kola_utils::interner::{PathInterner, StrInterner};

use crate::{
    env::{GlobalTypeEnv, KindEnv, ModuleTypeEnv},
    error::TypeErrors,
    phase::TypedNodes,
    prelude::{Constraints, Substitutable, Substitution},
    typer::Typer,
};

pub fn mocked_source() -> SourceId {
    let mut interner = PathInterner::new();
    interner.intern(Utf8PathBuf::from("test"))
}

pub fn mocked_spans(source_id: SourceId, tree: &impl TreeView) -> Locations {
    let span = Loc::new(source_id, Span::new(0, 0));
    tree.metadata_with(|node| Meta::default_with(span, node.kind()))
}

pub fn run_typer<T>(tree: TreeBuilder, root_id: Id<T>) -> Result<TypedNodes, Located<TypeErrors>>
where
    Id<T>: Visitable<TreeBuilder>,
{
    let source_id = mocked_source();
    let spans = Rc::new(mocked_spans(source_id, &tree));

    let type_env = GlobalTypeEnv::new();
    let module_scope = ModuleTypeEnv::new();
    let interner = StrInterner::new(); // TODO for tests with builtin types the interner should be passed
    let resolved = ResolvedNodes::new();

    let mut cons = Constraints::new();
    let typer = Typer::new(
        root_id,
        spans,
        &mut cons,
        &module_scope,
        &type_env,
        &interner,
        &resolved,
    );

    let mut types = typer.run(&tree, &mut Report::new()).unwrap();

    let mut subs = Substitution::empty();
    let mut kind_env = KindEnv::new();

    cons.solve(&mut subs, &mut kind_env)?;
    types.apply_mut(&mut subs);

    Ok(types)
}
