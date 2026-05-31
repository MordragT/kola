use kola_ir::instr::Func;

use crate::env::EnvIdx;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Closure {
    pub env: EnvIdx,
    pub func: Func,
}

const _: () = assert!(Closure::BYTES <= 12);

impl Closure {
    pub const BYTES: usize = std::mem::size_of::<Self>();

    #[inline]
    pub fn new(env: EnvIdx, func: Func) -> Self {
        Self { env, func }
    }
}
