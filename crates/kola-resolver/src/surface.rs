use kola_span::Loc;
use kola_tree::{
    node::{
        AnyId, ModuleName, ModuleType, ModuleTypeName, TypeName, TypeScheme, ValueName, Vis,
    },
    prelude::Id,
};

use crate::symbol::{AnySym, ModuleSym};

/// One public key = (namespace, source name).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Key {
    Value(ValueName),
    Type(TypeName),
    Module(ModuleName),
    ModuleType(ModuleTypeName),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Definition {
    pub sym: AnySym,
    /// Source identity. Not redundant with `sym`: a `using` forwarder has a
    /// fresh symbol but keeps the provider's origin.
    pub origin: AnySym,
    pub vis: Vis,
    /// Where the defining syntax lives, for printing and diagnostics.
    pub site: (ModuleSym, AnyId),
    pub body: DefinitionBody,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefinitionBody {
    /// An ordinary definition in `site`'s tree.
    Source(AnyId),
    /// A `using` forwarder: resolve `key` in `from`.
    Forward { from: ModuleSym, key: Key },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Obligation {
    pub loc: Loc,
    pub declared: ObligationKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObligationKind {
    Value(Id<TypeScheme>),
    Module(Id<ModuleType>),
}

/// A module's resolved surface: one definition and one obligation per key.
///
/// Both vectors are kept sorted by [`Key`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleSurface {
    pub defined: Vec<(Key, Definition)>,
    pub obligations: Vec<(Key, Obligation)>,
}

impl ModuleSurface {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, key: Key, definition: Definition) {
        match self.defined.binary_search_by_key(&key, |(key, _)| *key) {
            Ok(index) => self.defined[index].1 = definition,
            Err(index) => self.defined.insert(index, (key, definition)),
        }
    }

    pub fn obligate(&mut self, key: Key, obligation: Obligation) {
        match self
            .obligations
            .binary_search_by_key(&key, |(key, _)| *key)
        {
            Ok(index) => self.obligations[index].1 = obligation,
            Err(index) => self.obligations.insert(index, (key, obligation)),
        }
    }

    #[inline]
    pub fn definition(&self, key: Key) -> Option<&Definition> {
        self.defined
            .binary_search_by_key(&key, |(key, _)| *key)
            .ok()
            .map(|index| &self.defined[index].1)
    }

    #[inline]
    pub fn obligation(&self, key: Key) -> Option<&Obligation> {
        self.obligations
            .binary_search_by_key(&key, |(key, _)| *key)
            .ok()
            .map(|index| &self.obligations[index].1)
    }

    /// Declared obligations that no definition satisfies yet.
    pub fn outstanding(&self) -> impl Iterator<Item = &(Key, Obligation)> {
        self.obligations
            .iter()
            .filter(|(key, _)| self.definition(*key).is_none())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use camino::Utf8PathBuf;
    use kola_interner::{PathInterner, StrInterner};
    use kola_span::{Loc, SourceId, Span};
    use kola_tree::node::{AnyId, ValueName, Vis};
    use kola_tree::prelude::Id;

    use super::*;
    use crate::symbol::{AnySym, ModuleSym};

    fn loc() -> Loc {
        static SOURCE: OnceLock<SourceId> = OnceLock::new();
        let source = *SOURCE.get_or_init(|| {
            PathInterner::default().intern(Utf8PathBuf::from("test"))
        });
        Loc::new(source, Span::new(0, 0))
    }

    fn definition() -> Definition {
        let sym = AnySym::Module(ModuleSym::new());
        let id = AnyId::ValueBind(Id::new(0));

        Definition {
            sym,
            origin: sym,
            vis: Vis::Public,
            site: (ModuleSym::new(), id),
            body: DefinitionBody::Source(id),
        }
    }

    #[test]
    fn definition_satisfies_obligation() {
        let mut interner = StrInterner::default();
        let key = Key::Value(ValueName::new(interner.intern("x")));
        let mut surface = ModuleSurface::new();

        surface.obligate(
            key,
            Obligation {
                loc: loc(),
                declared: ObligationKind::Value(Id::new(0)),
            },
        );

        assert_eq!(surface.outstanding().count(), 1);

        surface.define(key, definition());

        assert_eq!(surface.outstanding().count(), 0);
        assert!(surface.definition(key).is_some());
    }

    #[test]
    fn keys_are_sorted() {
        let mut interner = StrInterner::default();
        let a = Key::Value(ValueName::new(interner.intern("a")));
        let b = Key::Value(ValueName::new(interner.intern("b")));
        let c = Key::Value(ValueName::new(interner.intern("c")));

        let mut surface = ModuleSurface::new();
        surface.define(c, definition());
        surface.define(a, definition());
        surface.define(b, definition());

        let keys = surface
            .defined
            .iter()
            .map(|(key, _)| *key)
            .collect::<Vec<_>>();

        assert!(keys.windows(2).all(|window| window[0] < window[1]));
    }
}
