use derive_more::From;
use kola_ir::instr::Func;

use crate::env::Env;

#[derive(Debug, From, Clone, PartialEq)]
pub struct Closure {
    pub env: Env,
    pub func: Func,
}

impl Closure {
    #[inline]
    pub fn new(env: Env, func: Func) -> Self {
        Self { env, func }
    }
}
