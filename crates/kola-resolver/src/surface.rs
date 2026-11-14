use kola_span::Loc;
use kola_tree::{
    node::{
        ModuleBind, ModuleName, ModuleType, ModuleTypeBind, ModuleTypeName, TypeBind, TypeName,
        TypeScheme, ValueBind, ValueName,
    },
    prelude::Id,
};

use crate::symbol::{ModuleSym, ModuleTypeSym, TypeSym, ValueSym};

/// A definition at a typed source site.
///
/// `origin` is the identity used for conflict checks. It is equal to `sym` for
/// source definitions; a `using` supplied definition may keep the provider's
/// origin.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Definition<S, B> {
    pub sym: S,
    pub origin: S,
    pub site: (ModuleSym, Id<B>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Obligation<D> {
    pub loc: Loc,
    pub declared: D,
}

/// The public definitions of one namespace, sorted by key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definitions<K, S, B> {
    pub entries: Vec<(K, Definition<S, B>)>,
}

impl<K, S, B> Default for Definitions<K, S, B> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<K: Ord + Copy, S, B> Definitions<K, S, B> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, key: K, definition: Definition<S, B>) {
        match self.entries.binary_search_by(|(k, _)| k.cmp(&key)) {
            Ok(index) => self.entries[index].1 = definition,
            Err(index) => self.entries.insert(index, (key, definition)),
        }
    }

    #[inline]
    pub fn definition(&self, key: K) -> Option<&Definition<S, B>> {
        self.entries
            .binary_search_by(|(k, _)| k.cmp(&key))
            .ok()
            .map(|index| &self.entries[index].1)
    }
}

/// The abstract obligations of one namespace, sorted by key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obligations<K, D> {
    pub entries: Vec<(K, Obligation<D>)>,
}

impl<K, D> Default for Obligations<K, D> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<K: Ord + Copy, D> Obligations<K, D> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn obligate(&mut self, key: K, obligation: Obligation<D>) {
        match self.entries.binary_search_by(|(k, _)| k.cmp(&key)) {
            Ok(index) => self.entries[index].1 = obligation,
            Err(index) => self.entries.insert(index, (key, obligation)),
        }
    }

    #[inline]
    pub fn obligation(&self, key: K) -> Option<&Obligation<D>> {
        self.entries
            .binary_search_by(|(k, _)| k.cmp(&key))
            .ok()
            .map(|index| &self.entries[index].1)
    }

    /// Declared obligations that the given definitions do not satisfy yet.
    pub fn outstanding<'a, S, B>(
        &'a self,
        definitions: &'a Definitions<K, S, B>,
    ) -> impl Iterator<Item = &'a (K, Obligation<D>)> + 'a {
        self.entries
            .iter()
            .filter(move |(key, _)| definitions.definition(*key).is_none())
    }
}

/// The public members of a module, split by role and namespace.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleMembers {
    pub definitions: ModuleDefinitions,
    pub obligations: ModuleObligations,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleDefinitions {
    pub values: Definitions<ValueName, ValueSym, ValueBind>,
    pub types: Definitions<TypeName, TypeSym, TypeBind>,
    pub modules: Definitions<ModuleName, ModuleSym, ModuleBind>,
    pub module_types: Definitions<ModuleTypeName, ModuleTypeSym, ModuleTypeBind>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleObligations {
    pub values: Obligations<ValueName, Id<TypeScheme>>,
    pub modules: Obligations<ModuleName, Id<ModuleType>>,
}

impl ModuleMembers {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn outstanding_values<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a (ValueName, Obligation<Id<TypeScheme>>)> + 'a {
        self.obligations
            .values
            .outstanding(&self.definitions.values)
    }

    pub fn outstanding_modules<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a (ModuleName, Obligation<Id<ModuleType>>)> + 'a {
        self.obligations
            .modules
            .outstanding(&self.definitions.modules)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use camino::Utf8PathBuf;
    use kola_interner::{PathInterner, StrInterner};
    use kola_span::{Loc, SourceId, Span};
    use kola_tree::node::ValueName;

    use super::*;

    fn loc() -> Loc {
        static SOURCE: OnceLock<SourceId> = OnceLock::new();
        let source =
            *SOURCE.get_or_init(|| PathInterner::default().intern(Utf8PathBuf::from("test")));
        Loc::new(source, Span::new(0, 0))
    }

    fn definition() -> Definition<ValueSym, ValueBind> {
        let sym = ValueSym::new();
        Definition {
            sym,
            origin: sym,
            site: (ModuleSym::new(), Id::new(0)),
        }
    }

    #[test]
    fn definition_satisfies_obligation() {
        let mut interner = StrInterner::default();
        let key = ValueName::new(interner.intern("x"));
        let mut definitions: Definitions<ValueName, ValueSym, ValueBind> = Definitions::new();
        let mut obligations: Obligations<ValueName, Id<TypeScheme>> = Obligations::new();

        obligations.obligate(
            key,
            Obligation {
                loc: loc(),
                declared: Id::new(0),
            },
        );

        assert_eq!(obligations.outstanding(&definitions).count(), 1);

        definitions.define(key, definition());

        assert_eq!(obligations.outstanding(&definitions).count(), 0);
        assert!(definitions.definition(key).is_some());
        assert!(obligations.obligation(key).is_some());
    }

    #[test]
    fn keys_are_sorted() {
        let mut interner = StrInterner::default();
        let a = ValueName::new(interner.intern("a"));
        let b = ValueName::new(interner.intern("b"));
        let c = ValueName::new(interner.intern("c"));

        let mut definitions: Definitions<ValueName, ValueSym, ValueBind> = Definitions::new();
        definitions.define(c, definition());
        definitions.define(a, definition());
        definitions.define(b, definition());

        let keys = definitions
            .entries
            .iter()
            .map(|(key, _)| *key)
            .collect::<Vec<_>>();

        assert!(keys.windows(2).all(|window| window[0] < window[1]));
    }
}
