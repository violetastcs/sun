mod lexer;
mod sexpr;
mod parser;
mod analysis;

const SRC: &str = include_str!("../examples/main.sun");

fn main() {
    let code = parser::parse(SRC)
        .unwrap();

    let info = analysis::analyze_unit(&code);

    dbg!(info);
}
