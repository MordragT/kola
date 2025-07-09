use std::collections::HashMap;

use kola_span::{Loc, Located};
use kola_utils::errors::Errors;
use log::trace;

use crate::{
    error::TypeError,
    prelude::{Substitutable, Substitution},
    types::{MonoType, TypeVar, Typed},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Obligation {
    Exists(Loc),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obligations(HashMap<TypeVar, Obligation>);

impl Obligations {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, var: TypeVar, obligation: Obligation) {
        self.0.insert(var, obligation);
    }

    pub fn get(&self, var: &TypeVar) -> Option<&Obligation> {
        self.0.get(var)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn require_exists(&mut self, var: TypeVar, loc: Loc) {
        let obligation = Obligation::Exists(loc);
        self.insert(var, obligation);
    }

    pub fn verify(self, s: &mut Substitution) -> Result<(), Errors<Located<TypeError>>> {
        let mut errors = Errors::new();

        for (var, obligation) in self.0 {
            match obligation {
                Obligation::Exists(loc) => {
                    // The idea here is that at this point the type variable result
                    // must be fully resolved.

                    // TODO maybe the naming isn't that great,
                    // because it doesn't really check existence,
                    // but rather that it is fully resolved which is a stronger condition.

                    let result_t = MonoType::Var(var);
                    let actual = result_t.apply_cow(s);

                    trace!("EXISTS: {}", var);

                    if !actual.type_vars().is_empty() {
                        errors.push((TypeError::NotExistent(var), loc));
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
