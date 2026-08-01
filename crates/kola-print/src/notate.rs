use bumpalo::Bump;

use super::{
    notation::Notation,
    printer::{PrintOptions, Printer},
};

/// A printer that can render a value of type `T` into a [`Notation`].
///
/// The trait is implemented by the *context* (e.g. a tree printer), not by the
/// value being printed. This lets a single context render many different value
/// types, and lets the derive macro generate `impl Notate<'a, T> for Cx`
/// without needing to know what `Cx` is.
pub trait Notate<'a, T> {
    fn notate(&self, value: &T, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, value: &T, options: PrintOptions, arena: &'a Bump) -> std::string::String {
        let notation = self.notate(value, arena);
        let mut printer = Printer::new(&notation, options, arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, arena).unwrap();

        output
    }

    fn print(&self, value: &T, options: PrintOptions, arena: &'a Bump) {
        let rendered = self.render(value, options, arena);
        println!("{rendered}");
    }
}
