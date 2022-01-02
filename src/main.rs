mod lexer;
mod sexpr;
mod parser;

const SRC: &str = include_str!("../examples/main.sun");

fn main() {
    dbg!(parser::parse(SRC)).unwrap();
}
