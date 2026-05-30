use derive_more::From;
use kola_ir::instr::Func;

use crate::{
    env::{HeapEnv, RawEnv},
    heap::Heap,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapClosure {
    pub env: HeapEnv,
    pub func: Func,
}

impl HeapClosure {
    #[inline]
    pub fn get(self, heap: &Heap) -> RawClosure<'_> {
        let env = heap.get_env(self.env);
        RawClosure::new(env, self.func)
    }
}

#[derive(Debug, From, Clone, PartialEq)]
pub struct RawClosure<'a> {
    pub env: RawEnv<'a>,
    pub func: Func,
}

impl<'a> RawClosure<'a> {
    #[inline]
    pub fn new(env: RawEnv<'a>, func: Func) -> Self {
        Self { env, func }
    }

    #[inline]
    pub fn into_owned(self) -> RawClosure<'static> {
        let env = self.env.into_owned();
        RawClosure::new(env, self.func)
    }

    #[inline]
    pub fn alloc(&self, heap: &mut Heap) -> HeapClosure {
        let env = heap.alloc_env(&self.env);
        HeapClosure {
            env,
            func: self.func,
        }
    }
}
