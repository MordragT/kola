use chumsky::Parser;
use kola::syntax::lexer::lexer;
use std::fs;

fn main() {
    let src = fs::read_to_string("examples/main.kl").unwrap();

    let tokens = lexer().parse(&src).into_result().unwrap();

    println!("{tokens:?}")
}
