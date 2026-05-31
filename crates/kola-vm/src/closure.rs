use kola_ir::instr::Func;

use crate::env::EnvIdx;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Closure {
    pub env: EnvIdx,
    pub func: Func,
}

impl Closure {
    #[inline]
    pub fn new(env: EnvIdx, func: Func) -> Self {
        Self { env, func }
    }
}
