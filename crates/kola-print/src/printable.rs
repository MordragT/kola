use bumpalo::Bump;

use super::{
    notation::Notation,
    printer::{PrintOptions, Printer},
};

pub trait Printable<With> {
    fn notate<'a>(&'a self, with: &'a With, arena: &'a Bump) -> Notation<'a>;

    fn render_with(&self, with: &With, options: PrintOptions) -> std::string::String {
        let arena = Bump::new();

        let notation = self.notate(with, &arena);
        let mut printer = Printer::new(&notation, options, &arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, &arena).unwrap();

        return output;
    }

    fn render(&self, options: PrintOptions) -> std::string::String
    where
        With: Default,
    {
        let with = With::default();
        self.render_with(&with, options)
    }

    fn print(&self, options: PrintOptions)
    where
        With: Default,
    {
        let rendered = self.render(options);
        println!("{rendered}");
    }

    fn print_with(&self, with: &With, options: PrintOptions) {
        let rendered = self.render_with(&with, options);
        println!("{rendered}");
    }
}
