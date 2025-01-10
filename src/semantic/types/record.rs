use std::fmt;

use crate::{
    semantic::{
        error::{InferError, InferResult},
        merge, Cache, Constraints, Context, Kind, Substitutable, Substitution, Unify,
    },
    syntax::ast::Ident,
};

use super::{MonoType, TypeVar, Typed};

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Property {
    pub k: Ident,
    pub v: MonoType,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.k, self.v)
    }
}

impl Substitutable for Property {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        self.v.try_apply(s, cache).map(|v| Property {
            k: self.k.clone(),
            v,
        })
    }
}

// An extensible record type.
///
/// A record is either `Empty`, meaning it has no properties,
/// or it is an extension of a record.
///
/// A record may extend what is referred to as a *record
/// variable*. A record variable is a type variable that
/// represents an unknown record type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordType {
    /// A record that has no properties.
    Empty,
    /// Extension of a record.
    Extension {
        /// The [`Property`] that extends the record type.
        head: Property,
        /// `tail` is the record variable.
        tail: MonoType,
    },
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Extension { head, tail } => write!(f, "{{ {head} | {tail} }}"),
        }
    }
}

/*
Rule (eq-head) defines two rows as equal
when their heads and tails are equal. The rule (eq-swap)is the most
interesting: it states that the first two fields of a row can be swapped
if (and only if) their labels are different. Together with transitivity
(eq-trans) and row equality (eq-head), this effectively allows us
to swap a field repeatedly to the front of a record, but not past an
equal label. With the new notion of equality, we can immediately
derive that:
{x :: Int, y :: Int} ∼= {y :: Int, x :: Int}
*/

impl Unify<&Self> for RecordType {
    // Below are the rules for record unification. In what follows monotypes
    // are denoted using lowercase letters, and type variables are denoted
    // by a lowercase letter preceded by an apostrophe `'`.
    //
    // `t = u` is read as:
    //
    //     type t unifies with type u
    //
    // `t = u => a = b` is read as:
    //
    //     if t unifies with u, then a must unify with b
    //
    // 1. Two empty records always unify, producing an empty substitution.
    // 2. {a: t | 'r} = {b: u | 'r} => error
    // 3. {a: t | 'r} = {a: u | 'r} => t = u
    // 4. {a: t |  r} = {a: u |  s} => t = u, r = s
    // 5. {a: t |  r} = {b: u |  s} => r = {b: u | 'v}, s = {a: t | 'v}
    //
    // Note rule 2. states that if two records extend the same type variable
    // they must have the same property name otherwise they cannot unify.
    //
    // self represents the expected type.
    fn unify(&self, with: &Self, ctx: &mut Context) {
        todo!()
    }
}

impl Typed for RecordType {
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> InferResult<()> {
        todo!()
    }

    fn contains(&self, tv: TypeVar) -> bool {
        match self {
            Self::Empty => false,
            Self::Extension { head, tail } => head.v.contains(tv) || tail.contains(tv),
        }
    }

    fn type_vars(&self, vars: &mut Vec<TypeVar>) {
        match self {
            Self::Empty => (),
            Self::Extension { head, tail } => {
                head.v.type_vars(vars);
                tail.type_vars(vars);
            }
        }
    }
}

impl Substitutable for RecordType {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        match self {
            Self::Empty => None,
            Self::Extension { head, tail } => {
                let h = head.try_apply(s, cache);
                let t = tail.try_apply(s, cache);

                merge(h, || head.clone(), t, || tail.clone())
                    .map(|(head, tail)| Self::Extension { head, tail })
            }
        }
    }
}

// Property
// pub type FieldType = (Ident, MonoType);

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct RecordType {
//     /// We write the type of a record as a sequence of labeled types
//     pub fields: Vec<FieldType>,
//     /// Following Gaster and Jones [7], we
//     /// consider an extensible row calculus where a row is either empty
//     /// or an extension of a row.
//     pub tail: Option<Box<MonoType>>,
// }

// pub struct FieldComparison {
//     pub union: Vec<FieldType>,
//     pub lhs: Vec<FieldType>,
//     pub rhs: Vec<FieldType>,
// }

// impl RecordType {
//     // eq-head

//     // eq-swap

//     // / Compares the fields in self with other
//     // / Returns the fields both in self and other (union),
//     // / as well as fields that are in self but not in other and vice versa.
//     // pub fn compare_fields(&self, other: &Self) -> FieldComparison {
//     //     for (n1, ty1) in &self.fields {

//     //     }
//     // }

//     // /// Removes the intersection of self and other from both
//     // /// and returns it.
//     // pub fn symmetric_intersection_removal_by_field_name(
//     //     &mut self,
//     //     other: &mut Self,
//     // ) -> Vec<(Ident, Type, Type)> {
//     //     let mut intersection = Vec::new();

//     //     for (n1, ty1) in self.fields.drain(..) {
//     //         if let Some(i) = other.fields.iter().
//     //     }
//     // }
// }

// impl fmt::Display for RecordType {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{{\n")?;

//         for (name, ty) in &self.fields {
//             write!(f, "\t{name} : {ty}\n")?;
//         }

//         if let Some(tail) = &self.tail {
//             write!(f, "\t| {tail}\n")?;
//         }

//         write!(f, "}}\n")
//     }
// }
