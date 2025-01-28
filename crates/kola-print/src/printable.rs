use bumpalo::Bump;

use super::{
    notation::Notation,
    printer::{PrintOptions, Printer},
};

pub trait Printable<With> {
    fn notate<'a>(&'a self, with: &'a With, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, with: &With, arena: &Bump, options: PrintOptions) -> std::string::String {
        let notation = self.notate(with, arena);
        let mut printer = Printer::new(&notation, options, arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, arena).unwrap();

        return output;
    }
}
