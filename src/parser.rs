use std::str::FromStr;

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

fn expect(sexpr: &Sexpr, kind: AtomKind) -> Result<&Atom, ParserError> {
    if sexpr.atom.kind() == kind {
        Ok(&sexpr.atom)
    } else {
        Err(ParserError {
            kind: ParserErrorKind::Expected(kind, sexpr.atom.kind()),
            span: sexpr.span
        })
    }
}

fn expect_symbol(sexpr: &Sexpr) -> Result<&String, ParserError> {
    let symbol = expect(sexpr, AtomKind::Symbol)?;

    match symbol {
        Atom::Symbol(s) => Ok(s),
        _ => unreachable!()
    }
}

fn expect_list(sexpr: &Sexpr) -> Result<&Vec<Sexpr>, ParserError> {
    let symbol = expect(sexpr, AtomKind::List)?;

    match symbol {
        Atom::List(s) => Ok(s),
        _ => unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Symbol(String),
    Integer(i128),
    Real(f64),
    String(String),
    Quoted(Box<Sexpr>),
    Empty,

    Call {
        func: String,
        args: Vec<Expression> 
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
    span: Span
}

fn parse_expr(sexpr: Sexpr) -> Result<Expression, ParserError> {
    match sexpr {
        Sexpr { atom: Atom::Symbol(i), span } => Ok(Expression {
            kind: ExpressionKind::Symbol(i),
            span: span
        }),
        Sexpr { atom: Atom::Integer(i), span } => Ok(Expression {
            kind: ExpressionKind::Integer(i),
            span: span
        }),
        Sexpr { atom: Atom::Real(i), span } => Ok(Expression {
            kind: ExpressionKind::Real(i),
            span: span
        }),
        Sexpr { atom: Atom::String(i), span } => Ok(Expression {
            kind: ExpressionKind::String(i),
            span: span
        }),
        Sexpr { atom: Atom::Quoted(i), span } => Ok(Expression {
            kind: ExpressionKind::Quoted(i),
            span: span
        }),

        Sexpr { atom: Atom::List(items), span } => {
            match &items[..] {
                [a, rest @ ..] => {
                    let fun = expect_symbol(a)?;

                    let mut args = vec![];

                    for arg in rest {
                        args.push(parse_expr(arg.clone())?);
                    }

                    Ok(Expression {
                        kind: ExpressionKind::Call {
                            func: fun.clone(),
                            args: args
                        },
                        span: span
                    })
                },

                [] => Ok(Expression {
                    kind: ExpressionKind::Empty,
                    span: span
                })
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InvalidBaseType;

#[derive(Debug, Clone)]
pub enum BaseType {
    U8, I8,
    U16, I16,
    U32, I32,
    U64, I64,

    F32, F64,

    Char,
}

impl FromStr for BaseType {
    type Err = InvalidBaseType;

    fn from_str(s: &str) -> Result<BaseType, InvalidBaseType> {
        match s {
            "u8" => Ok(BaseType::U8),
            "i8" => Ok(BaseType::I8),
            "u16" => Ok(BaseType::U16),
            "i16" => Ok(BaseType::I16),
            "u32" => Ok(BaseType::U32),
            "i32" => Ok(BaseType::I32),
            "u64" => Ok(BaseType::U64),
            "i64" => Ok(BaseType::I64),
            
            _ => Err(InvalidBaseType)
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeData {
    Empty,
    BaseType(BaseType),
    TypeRef(String),
    Fun {
        args: Vec<Type>,
        ret: Box<Type>,
    },
    Generic {
        of: String,
        args: Vec<Type>
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    data: TypeData,
    span: Span
}

fn parse_type(sexpr: Sexpr) -> Result<Type, ParserError> {
    match sexpr {
        Sexpr { atom: Atom::Symbol(s), span } => {
            match &s[..] {
                s if BaseType::from_str(s).is_ok() => Ok(Type {
                    data: TypeData::BaseType(s.parse().unwrap()),
                    span: span
                }),

                s => Ok(Type {
                    data: TypeData::TypeRef(s.into()),
                    span: span
                })
            }
        },

        Sexpr { atom: Atom::List(items), span } => {
            let of = expect_symbol(&items[0])?;
            let rest = &items[1..];

            match &of[..] {
                "Fun" => {
                    let args_sexpr = expect_list(&rest[0])?;
                    let mut args = vec![];

                    for arg in args_sexpr.iter().cloned() {
                        args.push(parse_type(arg)?);
                    }

                    let ret = parse_type(rest[1].clone())?;

                    Ok(Type {
                        data: TypeData::Fun {
                            args: args,
                            ret: Box::new(ret)
                        },
                        span: span
                    })
                },

                of => {
                    let mut args = vec![];

                    for arg in rest.iter().cloned() {
                        args.push(parse_type(arg)?);
                    }
        
                    Ok(Type {
                        data: TypeData::Generic {
                            of: of.into(),
                            args: args
                        },
                        span: span
                    })
                }
            }
        },

        _ => todo!()
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Defun {
        name: String,
        args: Vec<String>,
        body: Vec<Expression>
    },
    Decl {
        name: String,
        kind: Type
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
                [sexpr, rest @ ..] => {
                    let kind = expect_symbol(sexpr)?;

                    match &kind[..] {
                        "defun" => {
                            let name = expect_symbol(&rest[0])?;
                            let arg_sexpr = expect_list(&rest[1])?;
                            let arg_list = arg_sexpr
                                .iter()
                                .map(|s| expect_symbol(s));
        
                            let mut args = vec![];
        
                            for arg in arg_list {
                                args.push(arg?.clone());
                            }
        
                            let body_sexprs = &rest[2..];
                            let mut body = vec![];

                            for sexpr in body_sexprs.iter().cloned() {
                                body.push(parse_expr(sexpr)?);
                            }
        
                            Ok(Statement {
                                kind: StatementKind::Defun {
                                    name: name.clone(),
                                    args: args,
                                    body: body
                                },
                                span: span
                            })
                        },

                        ":" => {
                            let name = expect_symbol(&rest[0])?;
                            let kind = parse_type(rest[1].clone())?;

                            Ok(Statement {
                                kind: StatementKind::Decl {
                                    name: name.into(),
                                    kind: kind
                                },
                                span: span
                            })
                        },

                        s => todo!("{}", s)
                    }
                },

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