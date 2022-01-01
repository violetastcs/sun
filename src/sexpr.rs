use codespan::Span;

#[derive(Debug, Clone)]
pub enum Atom {
    Symbol(String),
    Integer(i128)
}

#[derive(Debug, Clone)]
pub struct Sexpr {
    atom: Atom,
    span: Span
}