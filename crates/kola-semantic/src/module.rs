use indexmap::IndexMap;
use kola_tree::node::Symbol;

use crate::env::{KindEnv, TypeEnv};

/*

*   For functors like `functor (S : SIG) => Body`, `S` acts precisely like a **Module Variable**.
    It's a name that represents an unknown module *argument* that is required to conform to the signature `SIG`.
*   When you *apply* this functor to a concrete module, say `MyFunctor(MyModule)`,
    conceptually, you are indeed "substituting" `S` with `MyModule` inside the `Body`.
    The most common and elegant way to implement this is not through literal text or AST substitution,
    but by **extending the environment**.
    When you process the `Body` of the functor application `MyFunctor(MyModule)`,
    you perform this processing in an environment where the name `S` is bound to the module `MyModule`.
*   This environment binding means that any path like `S.x` or `S.T` encountered within the `Body`
    is resolved by first looking up `S` in the environment (finding `MyModule`)
    and then looking up `x` or `T` within `MyModule`.
*/

pub struct ModuleEnv {
    modules: IndexMap<Symbol, ModuleType>,
}

pub struct ModuleType {
    types: TypeEnv,
    kinds: KindEnv,
    modules: ModuleEnv,
}
