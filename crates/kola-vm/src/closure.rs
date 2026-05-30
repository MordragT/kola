use kola_ir::instr::Func;

use crate::{
    env::{HeapEnv, RawEnv},
    heap::Heap,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Closure {
    pub env: HeapEnv,
    pub func: Func,
}

impl Closure {
    #[inline]
    pub fn new(env: HeapEnv, func: Func) -> Self {
        Self { env, func }
    }

    #[inline]
    pub fn get_env(self, heap: &Heap) -> RawEnv<'_> {
        heap.get_env(self.env)
    }
}
