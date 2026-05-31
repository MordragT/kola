use std::borrow::Cow;

use kola_ir::{
    id::Id,
    instr::{Expr, Symbol},
};

use crate::{
    arenas::RangeIdx, closure::Closure, env::HeapEnv, handler::HeapHandler, list::ListIdx,
    record::HeapRecord, string::StringIdx,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ContFrame {
    Bottom,
    /// A pure continuation frame (γ, x, N) closes a let-binding
    /// let x : Symbol = [ ] in N : Expr, over environment γ.
    Pure {
        var: Symbol,
        body: Id<Expr>,
        env: HeapEnv,
    },

    /// A handler closure (γ, H) closes a handler definition H over environment γ.
    ///
    /// Multiple handler closures might use the same handler definition but with different environments.
    Handler {
        handler: HeapHandler,
        env: HeapEnv,
    },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    NumRec {
        data: f64,
        step: Closure,
    },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    ListRec {
        data: ListIdx,
        step: Closure,
    },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    RecordRec {
        data: HeapRecord,
        step: Closure,
    },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    StrRec {
        data: StringIdx,
        step: Closure,
    },
}

impl ContFrame {
    pub fn num_rec(data: f64, step: Closure) -> Self {
        Self::NumRec { data, step }
    }

    pub fn list_rec(data: ListIdx, step: Closure) -> Self {
        Self::ListRec { data, step }
    }

    pub fn record_rec(data: HeapRecord, step: Closure) -> Self {
        Self::RecordRec { data, step }
    }

    pub fn str_rec(data: StringIdx, step: Closure) -> Self {
        Self::StrRec { data, step }
    }

    pub fn pure(var: Symbol, body: Id<Expr>, env: HeapEnv) -> Self {
        Self::Pure { var, body, env }
    }

    pub fn handler(handler: HeapHandler, env: HeapEnv) -> Self {
        Self::Handler { handler, env }
    }

    /// Creates the identity continuation frame  ([ ], (∅, {return x → x}))
    pub fn identity(env: HeapEnv) -> Self {
        Self::Handler {
            handler: HeapHandler::identity(),
            env,
        }
    }
}

/// A continuation κ consists of a stack of continuation frames
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapCont(pub RangeIdx<ContFrame>);

/// A continuation κ consists of a stack of continuation frames
#[derive(Debug, Clone, PartialEq)]
pub struct RawCont<'a>(pub Cow<'a, [ContFrame]>);

impl<'a> RawCont<'a> {
    pub fn empty() -> Self {
        Self(Cow::Borrowed(&[]))
    }

    /// Creates the identity continuation κ0 = [([ ], (∅, {return x → x}))]
    /// Returns continuation with single frame
    pub fn identity(handler_env: HeapEnv) -> Self {
        Self(Cow::Owned(vec![ContFrame::identity(handler_env)]))
    }

    pub fn push(&mut self, frame: ContFrame) {
        self.0.to_mut().push(frame);
    }

    pub fn pop(&mut self) -> Option<ContFrame> {
        self.0.to_mut().pop()
    }

    pub fn append(&mut self, cont: RawCont) {
        self.0.to_mut().extend_from_slice(&cont.0);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
