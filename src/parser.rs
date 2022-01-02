use codespan::Span;

use crate::lexer::Lexer;
use crate::sexpr::{self, Atom, AtomKind, Sexpr, SexprError, SexprErrorKind};

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    Sexpr(SexprErrorKind),
    ExpectedStatement,
    Expected(AtomKind, AtomKind)
}

#[derive(Debug, Clone)]
pub struct ParserError {
    kind: ParserErrorKind,
    span: Span
}

impl From<SexprError> for ParserError {
    fn from(SexprError { kind, span }: SexprError) -> ParserError {
        ParserError {
            kind: ParserErrorKind::Sexpr(kind),
            span: span
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Call {
        func: String,
        args: Vec<Expression> 
    },
    Add {
        lhs: Box<Expression>,
        rhs: Box<Expression>
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    span: Span
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Defun {
        name: String,
        args: Vec<String>,
        body: Vec<Expression>
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
    span: Span
}

fn parse_statement(sexpr: Sexpr) -> Result<Statement, ParserError> {
    let span = sexpr.span;
    
    match sexpr {
        Sexpr { atom: Atom::List(items), span } => {
            match &items[..] {
                [Sexpr { atom: Atom::Symbol(sym), span }, a @ ..] => {
                    match &sym[..] {
                        "defun" => todo!("defun"),
                        _ => todo!()
                    }
                },

                [Sexpr { atom, span }, ..] => Err(ParserError {
                    kind: ParserErrorKind::Expected(AtomKind::Symbol, atom.kind()),
                    span: span.clone()
                }),

                [] => Err(ParserError {
                    kind: ParserErrorKind::ExpectedStatement,
                    span: span
                })
            }
        }

        c => Err(ParserError {
            kind: ParserErrorKind::ExpectedStatement,
            span: span
        })
    }
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    statements: Vec<Statement>,
}

pub fn parse<'a>(src: &'a str) -> Result<CompilationUnit, ParserError> {
    let mut lexer = Lexer::from_src(src).peekable();
    
    let mut statements = vec![];

    loop {
        let next = sexpr::parse(&mut lexer);

        if next.is_err() {
            if let Err(SexprError { kind: SexprErrorKind::EndOfFile, .. }) = next {
                break;
            }
        }

        statements.push(parse_statement(next?)?);
    }

    Ok(CompilationUnit {
        statements: statements
    })
}