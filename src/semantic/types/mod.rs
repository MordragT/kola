use super::{error::InferError, Constraints, Kind};

pub use builtin::*;
pub use collection::*;
pub use function::*;
pub use mono::*;
pub use poly::*;
pub use record::*;
pub use var::*;

mod builtin;
mod collection;
mod function;
mod mono;
mod poly;
mod record;
mod var;

pub trait Typed {
    /// Validates that the current type meets the constraints of the specified kind.
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> Result<(), InferError>;
    /// occurs check
    fn contains(&self, tv: TypeVar) -> bool;
    fn type_vars(&self, vars: &mut Vec<TypeVar>);
}
