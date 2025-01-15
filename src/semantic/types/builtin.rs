use std::fmt;

use super::Typed;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Bool,
    Num,
    Char,
    Str,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Num => write!(f, "Num"),
            Self::Char => write!(f, "Char"),
            Self::Str => write!(f, "Str"),
        }
    }
}

impl Typed for BuiltinType {}
