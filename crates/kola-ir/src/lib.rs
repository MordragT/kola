// cps : continuation passing style
// ssa
// https://www.reddit.com/r/ProgrammingLanguages/comments/1ejyr0u/why_dont_we_use_continuation_passing_style_as/

/*
https://en.wikipedia.org/wiki/Continuation-passing_style
The key to CPS is to remember that
(a) every function takes an extra argument known as its continuation,
and (b) every argument in a function call must be either a variable
or a lambda expression (not a more complex expression).
This has the effect of turning expressions "inside-out"
because the innermost parts of the expression must be evaluated first,
thus CPS makes explicit the order of evaluation as well as the control flow.

https://overreacted.io/algebraic-effects-for-the-rest-of-us/
https://gist.github.com/yelouafi/57825fdd223e5337ba0cd2b6ed757f53
https://en.m.wikipedia.org/wiki/Delimited_continuation

https://en.wikipedia.org/wiki/A-normal_form
https://matt.might.net/articles/a-normalization/
*/

pub mod id;
pub mod instr;
pub mod ir;
pub mod print;

pub mod prelude {
    pub use crate::id::Id;
    pub use crate::instr;
    pub use crate::ir::{Ir, IrBuilder};
}
