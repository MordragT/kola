use bumpalo::Bump;

use super::{
    notation::Notation,
    printer::{PrintOptions, Printer},
};

pub trait Printable<With> {
    fn notate<'a>(&'a self, with: &'a With, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, with: &With, options: PrintOptions) -> std::string::String {
        let arena = Bump::new();

        let notation = self.notate(with, &arena);
        let mut printer = Printer::new(&notation, options, &arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, &arena).unwrap();

        return output;
    }

    fn print(&self, options: PrintOptions)
    where
        With: Default,
    {
        let with = With::default();
        let rendered = self.render(&with, options);
        println!("{rendered}");
    }

    fn print_with(&self, with: &With, options: PrintOptions) {
        let rendered = self.render(&with, options);
        println!("{rendered}");
    }
}
