use kola_ir::{
    id::Id,
    instr::{Expr, Symbol},
};

use crate::{
    closure::Closure, env::EnvIdx, handler::HeapHandler, list::ListIdx, record::RecordIdx,
    string::StringIdx,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ContFrame {
    /// A pure continuation frame (γ, x, N) closes a let-binding
    /// let x : Symbol = [ ] in N : Expr, over environment γ.
    Pure {
        var: Symbol,
        body: Id<Expr>,
        env: EnvIdx,
    },

    /// A handler closure (γ, H) closes a handler definition H over environment γ.
    ///
    /// Multiple handler closures might use the same handler definition but with different environments.
    Handler { handler: HeapHandler, env: EnvIdx },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    NumRec { data: f64, step: Closure },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    ListRec {
        data: Option<ListIdx>,
        step: Closure,
    },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    RecordRec { data: RecordIdx, step: Closure },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    StrRec { data: StringIdx, step: Closure },
}

const _: () = assert!(ContFrame::BYTES <= 32);

impl ContFrame {
    pub const BYTES: usize = std::mem::size_of::<Self>();

    pub fn num_rec(data: f64, step: Closure) -> Self {
        Self::NumRec { data, step }
    }

    pub fn list_rec(data: Option<ListIdx>, step: Closure) -> Self {
        Self::ListRec { data, step }
    }

    pub fn record_rec(data: RecordIdx, step: Closure) -> Self {
        Self::RecordRec { data, step }
    }

    pub fn str_rec(data: StringIdx, step: Closure) -> Self {
        Self::StrRec { data, step }
    }

    pub fn pure(var: Symbol, body: Id<Expr>, env: EnvIdx) -> Self {
        Self::Pure { var, body, env }
    }

    pub fn handler(handler: HeapHandler, env: EnvIdx) -> Self {
        Self::Handler { handler, env }
    }

    /// Creates the identity continuation frame  ([ ], (∅, {return x → x}))
    pub fn identity(env: EnvIdx) -> Self {
        Self::Handler {
            handler: HeapHandler::identity(),
            env,
        }
    }
}

// TODO: if I need this as a first class value then maybe just store Value::List(Value::ContFrame) ?

/// A continuation κ consists of a stack of continuation frames
#[derive(Debug, Clone, PartialEq)]
pub struct Cont(Vec<ContFrame>);

impl Cont {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    /// Creates the identity continuation κ0 = [([ ], (∅, {return x → x}))]
    /// Returns continuation with single frame
    #[inline]
    pub fn identity(handler_env: EnvIdx) -> Self {
        Self(vec![ContFrame::identity(handler_env)])
    }

    #[inline]
    pub fn push(&mut self, frame: ContFrame) {
        self.0.push(frame);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<ContFrame> {
        self.0.pop()
    }

    #[inline]
    pub fn append(&mut self, cont: &mut Cont) {
        self.0.append(&mut cont.0);
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
