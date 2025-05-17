use kola_ir::{id::Id, instr::Func};
use std::fmt;

use crate::{cont::Cont, env::Env};

/// Values produced by evaluating expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Char(char),
    Num(f64),
    Str(String),
    // A function closure (environment, function definition)
    Func(Env, Func),
    // A captured continuation
    Cont(Cont),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Func(_, _) => write!(f, "<function>"),
            Value::Cont(_) => write!(f, "<continuation>"),
        }
    }
}
