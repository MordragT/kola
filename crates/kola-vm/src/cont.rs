use crate::{
    env::Env,
    value::{Closure, List, Record},
};
use kola_ir::{
    id::Id,
    instr::{Expr, Func, HandlerClause, Symbol},
    ir::{Ir, IrView},
};
use kola_utils::interner::StrKey;

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnClause {
    /// Built-in identity function (x -> x)
    Identity,
    /// User-defined function from IR
    Function(Func),
}

/// Contains the behavior of the handler, which is a set of clauses.
///
/// ```
/// handle
///  some_computation()
/// with
///   return x -> x + 1       // This is the return clause
///   operation1 x k -> ...   // This is an operation clause
///   operation2 x k -> ...   // Another operation clause
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Handler {
    // The return clause handles the normal result when no effects are performed.
    pub return_clause: ReturnClause,
    pub op_clauses: Vec<(StrKey, Func)>,
}

impl Handler {
    pub fn identity() -> Self {
        Self {
            return_clause: ReturnClause::Identity,
            op_clauses: Vec::new(),
        }
    }

    pub fn function(return_clause: Func) -> Self {
        Self {
            return_clause: ReturnClause::Function(return_clause),
            op_clauses: Vec::new(),
        }
    }

    /// Create a handler from IR handler clauses
    pub fn from_clauses(head: Id<HandlerClause>, ir: &Ir) -> Self {
        // TODO Currently handler clauses cannot create a user defined return clause
        // therefore we start with the identity return clause.
        let mut handler = Handler::identity();

        // Walk the linked list of clauses
        for HandlerClause {
            op, param, body, ..
        } in ir.iter_clauses(Some(head))
        {
            // Create a function for this handler clause
            let handler_func = Func {
                param, // The operation argument parameter
                body,  // The handler body
            };

            // Add the operation clause to the handler
            handler.add_operation(op, handler_func);
        }

        handler
    }

    pub fn add_operation(&mut self, op: StrKey, handler: Func) {
        self.op_clauses.push((op, handler));
    }

    pub fn find_operation(&self, op: StrKey) -> Option<Func> {
        self.op_clauses
            .iter()
            .find(|(name, _)| *name == op)
            .map(|(_, handler)| *handler)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContFrame {
    /// A pure continuation frame (γ, x, N) closes a let-binding
    /// let x : Symbol = [ ] in N : Expr, over environment γ.
    Pure {
        var: Symbol,
        body: Id<Expr>,
        env: Env,
    },

    /// A handler closure (γ, H) closes a handler definition H over environment γ.
    ///
    /// Multiple handler closures might use the same handler definition but with different environments.
    Handler { handler: Handler, env: Env },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    NumRec { data: f64, step: Closure },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    ListRec { data: List, step: Closure },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    RecordRec { data: Record, step: Closure },

    /// A recursive continuation frame (data, step) is used for primitive recursion.
    StrRec { data: String, step: Closure },
}

impl ContFrame {
    pub fn num_rec(data: f64, step: Closure) -> Self {
        Self::NumRec { data, step }
    }

    pub fn list_rec(data: List, step: Closure) -> Self {
        Self::ListRec { data, step }
    }

    pub fn record_rec(data: Record, step: Closure) -> Self {
        Self::RecordRec { data, step }
    }

    pub fn str_rec(data: String, step: Closure) -> Self {
        Self::StrRec { data, step }
    }

    pub fn pure(var: Symbol, body: Id<Expr>, env: Env) -> Self {
        Self::Pure { var, body, env }
    }

    pub fn handler(handler: Handler, env: Env) -> Self {
        Self::Handler { handler, env }
    }

    /// Creates the identity continuation frame  ([ ], (∅, {return x → x}))
    pub fn identity(env: Env) -> Self {
        Self::Handler {
            handler: Handler::identity(),
            env,
        }
    }
}

/// A continuation κ consists of a stack of continuation frames
#[derive(Debug, Clone, PartialEq)]
pub struct Cont {
    frames: Vec<ContFrame>,
}

impl Cont {
    pub fn empty() -> Self {
        Self { frames: Vec::new() }
    }

    /// Creates the identity continuation κ0 = [([ ], (∅, {return x → x}))]
    pub fn identity(handler_env: Env) -> Self {
        // Return continuation with single frame
        Self {
            frames: vec![ContFrame::identity(handler_env)],
        }
    }

    pub fn push(&mut self, frame: ContFrame) {
        self.frames.push(frame);
    }

    pub fn pop(&mut self) -> Option<ContFrame> {
        self.frames.pop()
    }

    pub fn pop_or_identity(&mut self) -> ContFrame {
        if let Some(cont_frame) = self.frames.pop() {
            cont_frame
        } else {
            // If the stack is empty, return the identity frame
            ContFrame::identity(Env::new())
        }
    }

    pub fn top(&self) -> Option<&ContFrame> {
        self.frames.last()
    }

    pub fn top_mut(&mut self) -> Option<&mut ContFrame> {
        self.frames.last_mut()
    }

    pub fn append(&mut self, cont: &mut Cont) {
        self.frames.append(&mut cont.frames);
    }

    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ContFrame> {
        self.frames.iter()
    }
}
