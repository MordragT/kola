//! Liberating Effects with Rows and Handlers (4. Abstract Machine Semantics)
// https://matt.might.net/articles/cek-machines/

pub type Env = HashMap<Symbol, ir::ValueExpr>;

/// A handler closure (γ, H) closes a handler definition H over environment γ.
pub type Handler = (Env, ir::FuncExpr);

/// A pure continuation frame (γ, x, N) closes a let-binding
/// let x = [ ] in N over environment γ.
pub type PureContinuationFrame = (Env, ast::Symbol, ir::ValueExpr);

/// A pure continuation is a stack of pure continuation frames.
pub type PureContinuation = Vec<PureContinuationFrame>;

/// Intuitively, each continuation frame δ = (σ, χ) represents the pure continuation σ,
/// corresponding to a sequence of let bindings, inside a particular handler closure χ.
pub struct ContinuationFrame {
    pure: PureContinuation,
    handler: Handler,
}

/// A continuation κ consists of a stack of continuation frames
/// We choose to annotate captured continuations with their input type
pub struct Continuation {
    frames: Vec<ContinuationFrame>,
    // ty: MonoType,
}

impl Continuation {
    /// κ0 = [([ ], (∅, {return x → x}))]
    pub fn identity() -> Self {
        let x = IdentExpr::new(...);

        Self {
            frames: vec![ContinuationFrame {
                pure: Vec::new(),
                handler: (Env::new(), ir::FuncExpr {param: x, body: x.into()})
            }]
        }
    }
}

/// CEK machine operates on configurations which are triples of the form (C, E, K):
/// - C is the expression currently being evaluated
/// - The environment E binds the free variables
/// - The continuation K instructs the machine what to do once it is
///     done evaluating the current term in the C component.
pub type Configuration = (ir::Expr, Env, Continuation);

/// State Machine to interpret terms
pub struct Machine(Configuration);

impl Machine {
    /// The machine is initialised (M-INIT) by
    /// placing a term in a configuration alongside the empty environment
    /// and identity continuation κ0 .
    pub fn new(term: ir::Expr) -> Self {
        let config = (term, Env::new(), Continuation::identity());
        Self(config)
    }

    /// Transition Function
    pub fn step(&mut self) {
        todo!()
    }

    pub fn eval(&mut self) {
        todo!()
    }
}
