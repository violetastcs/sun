mod lexer;
mod sexpr;

const SRC: &str = include_str!("../examples/main.sun");

fn main() {
    let lexer = lexer::Lexer::from_src(SRC);

    for token in lexer {
        println!("{:?}", token);
    }
}
