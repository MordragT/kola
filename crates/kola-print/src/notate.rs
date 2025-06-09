use bumpalo::Bump;

use super::{
    notation::Notation,
    printer::{PrintOptions, Printer},
};

pub trait Notate<'a> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, options: PrintOptions, arena: &'a Bump) -> std::string::String {
        let notation = self.notate(&arena);
        let mut printer = Printer::new(&notation, options, &arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, &arena).unwrap();

        return output;
    }

    fn print(&self, options: PrintOptions, arena: &'a Bump) {
        let rendered = self.render(options, arena);
        println!("{rendered}");
    }
}
