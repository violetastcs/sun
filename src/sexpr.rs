use crate::lexer::{BracketKind, Token, TokenKind, Lexer};
use codespan::Span;

use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum SexprErrorKind {
    UnexpectedCloseBracket(BracketKind),
    UnclosedExpression,
    UnclosedString,
    Expected(&'static [TokenKind], Option<TokenKind>),
    EndOfFile
}

#[derive(Debug, Clone)]
pub struct SexprError {
    pub kind: SexprErrorKind,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum Atom {
    Symbol(String),
    Integer(i128),
    Real(f64),
    String(String),
    List(Vec<Sexpr>),
    Quoted(Box<Sexpr>)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AtomKind {
    Symbol,
    Integer,
    Real,
    String,
    List,
    Quoted
}

impl Atom {
    pub fn kind(&self) -> AtomKind {
        match &self {
            Atom::Symbol(_) => AtomKind::Symbol,
            Atom::Integer(_) => AtomKind::Integer,
            Atom::Real(_) => AtomKind::Real,
            Atom::String(_) => AtomKind::String,
            Atom::List(_) => AtomKind::List,
            Atom::Quoted(_) => AtomKind::Quoted,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sexpr {
    pub atom: Atom,
    pub span: Span
}

fn parse_list<'a>(lexer: &mut Peekable<Lexer<'a>>) -> Result<Sexpr, SexprError> {
    if let Some(Token { kind: TokenKind::OpenBracket(bracket_kind), span, .. }) = lexer.next() {
        let mut total_span = span;

        let mut closed = false;

        let mut items = vec![];

        loop {
            match lexer.peek().cloned() {
                Some(Token { kind: TokenKind::CloseBracket(kind), span, .. }) => {
                    if kind == bracket_kind {
                        lexer.next().unwrap();
                        total_span = total_span.merge(span);
                        closed = true;
                        break;
                    } else {
                        return Err(SexprError {
                            kind: SexprErrorKind::UnexpectedCloseBracket(kind),
                            span: span
                        })
                    }
                },

                None => break,

                _ => items.push(parse(lexer)?)
            }
        }

        if !closed {
            Err(SexprError {
                kind: SexprErrorKind::UnclosedExpression,
                span: total_span
            })
        } else {
            Ok(Sexpr {
                atom: Atom::List(items),
                span: total_span
            })
        }
    } else {
        unimplemented!()
    }
}

pub fn parse<'a>(lexer: &mut Peekable<Lexer<'a>>) -> Result<Sexpr, SexprError> {
    let next = lexer.peek();

    match next {
        Some(Token { kind: TokenKind::OpenBracket(..), .. }) => parse_list(lexer),

        Some(&Token { kind: TokenKind::Integer, data, span }) => {
            lexer.next().unwrap();
            
            Ok(Sexpr {
                atom: Atom::Integer(data.parse().unwrap()),
                span: span
            })
        },
        Some(&Token { kind: TokenKind::Real, data, span }) => {
            lexer.next().unwrap();
            
            Ok(Sexpr {
                atom: Atom::Real(data.parse().unwrap()),
                span: span
            })
        },

        Some(&Token { kind: TokenKind::Symbol, data, span }) => {
            lexer.next().unwrap();
            
            Ok(Sexpr {
                atom: Atom::Symbol(data.into()),
                span: span
            })
        }
        Some(&Token { kind: TokenKind::String, data, span }) => {
            lexer.next().unwrap();

            if data.chars().last() != Some('"') {
                return Err(SexprError {
                    kind: SexprErrorKind::UnclosedString,
                    span: span
                })
            }
            
            Ok(Sexpr {
                atom: Atom::String(data.into()),
                span: span
            })
        }

        Some(&Token { kind: TokenKind::Quote, span, .. }) => {
            lexer.next().unwrap();
            let quoted = parse(lexer)?;
            let span = span.merge(quoted.span);

            Ok(Sexpr {
                atom: Atom::Quoted(Box::new(quoted)),
                span: span
            })
        }

        Some(&Token { kind: TokenKind::Whitespace | TokenKind::Comment, data, span }) => {
            lexer.next().unwrap();
            parse(lexer)
        }

        None => Err(SexprError {
            kind: SexprErrorKind::EndOfFile,
            span: Span::new(
                0, 
                0
            )
        }),

        _ => unimplemented!()
    }
}