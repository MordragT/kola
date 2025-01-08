//! Algorithm W with HMF extension
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

use error::{InferError, InferResult};
use std::collections::HashMap;

pub use context::*;
pub use infer::*;
pub use scope::*;
pub use ty::*;

mod context;
pub mod error;
mod infer;
mod scope;
mod ty;

pub type Substitutions = HashMap<TypeVar, MonoType>;

/// Unify algorithm in J performs mutation, in W it does not.
/// This uses "in place" mutation similar to J but with the help of substitutions similar to w
/// Most general unifier, a substitution S such that S(lhs) is congruent to S(rhs).
pub fn unify(lhs: &mut MonoType, rhs: &mut MonoType, s: &mut Substitutions) -> InferResult<()> {
    unify_impl(lhs, rhs, s)?;

    lhs.substitute(s);
    rhs.substitute(s);

    Ok(())
}

fn unify_impl(lhs: &mut MonoType, rhs: &mut MonoType, s: &mut Substitutions) -> InferResult<()> {
    match (lhs, rhs) {
        (MonoType::Const(a), MonoType::Const(b)) if a == b => Ok(()),
        (MonoType::Var(tv), ty) => tv.bind(ty, s),
        (ty, MonoType::Var(tv)) => tv.bind(ty, s),
        (MonoType::Arrow(in1, out1), MonoType::Arrow(in2, out2)) => {
            unify_impl(in1, in2, s)?;
            out1.substitute(s);
            out2.substitute(s);
            unify_impl(out1, out2, s)?;
            Ok(())
        }
        (a, b) => Err(InferError::Failure(a.clone(), b.clone())),
    }
}
