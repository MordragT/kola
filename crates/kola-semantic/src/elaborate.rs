/// Relies on type inference for value binds, and inference relies on module environments to resolve path exprs.
/// The elaborator combines both and holds the data
///
/// TODO maybe Elaborator should borrow from "World" which in turn holds data of modules and ASTs etc.
pub struct Elaborator {}
