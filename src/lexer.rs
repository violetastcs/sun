use std::str::CharIndices;
use std::iter::Peekable;

use codespan::Span;
use regex::Regex;
use lazy_static::lazy_static;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    OpenBracket(BracketKind),
    CloseBracket(BracketKind),

    Quote,

    Integer,
    Real,

    Symbol,
    String,

    Whitespace,
    Comment
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BracketKind {
    Paren,
    Brace,
    Square
}

fn bracket_kind(c: char) -> Option<BracketKind> {
    match c {
        '(' | ')' => Some(BracketKind::Paren),
        '{' | '}' => Some(BracketKind::Brace),
        '[' | ']' => Some(BracketKind::Square),

        _ => None
    }
}

fn bracket(c: char) -> Option<TokenKind> {
    let bracket_kind = bracket_kind(c)?;

    match c {
        '(' | '{' | '[' => Some(TokenKind::OpenBracket(bracket_kind)),
        ')' | '}' | ']' => Some(TokenKind::CloseBracket(bracket_kind)),

        _ => None
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub data: &'a str,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub data: &'a str,
    stream: Peekable<CharIndices<'a>>
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let &(pos, next) = self.stream.peek()?;
        let start = pos;
        let mut end = start + next.len_utf8();

        match next {
            c if bracket(c).is_some() => {
                self.stream.next().unwrap();

                Some(Token {
                    kind: bracket(c).unwrap(),
                    data: &self.data[start..end],
                    span: Span::new(start as u32, end as u32)
                })
            }

            c if c.is_whitespace() => {
                while let Some((pos, c)) = self.stream.next_if(|(_,c)| c.is_whitespace()) {
                    end = pos + c.len_utf8()
                }

                Some(Token {
                    kind: TokenKind::Whitespace,
                    data: &self.data[start..end],
                    span: Span::new(start as u32, end as u32)
                })
            }

            '\'' => {
                self.stream.next().unwrap();

                Some(Token {
                    kind: TokenKind::Quote,
                    data: &self.data[start..end],
                    span: Span::new(start as u32, end as u32)
                })
            }

            '"' => {
                self.stream.next().unwrap();

                while let Some((pos, c)) = self.stream.next_if(|&(_,c)| c != '"') {
                    end = pos + c.len_utf8();

                    if c == '\\' {
                        if let Some((pos, c)) = self.stream.next() {
                            end = pos + c.len_utf8();
                        }
                    }
                }

                if let Some((pos, c)) = self.stream.next_if(|&(_,c)| c == '"') {
                    end = pos + c.len_utf8();
                }

                Some(Token {
                    kind: TokenKind::String,
                    data: &self.data[start..end],
                    span: Span::new(start as u32, end as u32)
                })
            }

            ';' => {
                while let Some((pos, c)) = self.stream.next_if(|&(_,c)| c != '\n') {
                    end = pos + c.len_utf8();
                }

                Some(Token {
                    kind: TokenKind::Comment,
                    data: &self.data[start..end],
                    span: Span::new(start as u32, end as u32)
                })
            }

            _ => self.lex_symbol()
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn from_src(src: &'a str) -> Lexer<'a> {
        Lexer { data: src, stream: src.char_indices().peekable() }
    }

    fn valid_symbol(c: char) -> bool {
        !c.is_whitespace() &&
        !bracket(c).is_some() &&
        c != '"' &&
        c != '\''
    }

    fn lex_symbol(&mut self) -> Option<Token<'a>> {
        let &(start, c) = self.stream.peek().unwrap();
        let mut end = start + c.len_utf8();

        while let Some((pos, c)) = self.stream.next_if(|&(_,c)| Lexer::valid_symbol(c)) {
            end = pos + c.len_utf8();
        }

        lazy_static! {
            static ref INT: Regex = Regex::new("^[\\-+]?[0-9]+$").unwrap();
            static ref REAL: Regex = Regex::new("^[\\-+]?[0-9]+((\\.[0-9]+)?([eE][\\-+]?[0-9]+)?)$").unwrap();
        }

        let data = &self.data[start..end];

        Some(Token {
            kind: if INT.is_match(data) {
                TokenKind::Integer
            } else if REAL.is_match(data) {
                TokenKind::Real
            } else {
                TokenKind::Symbol
            },
            data: data,
            span: Span::new(start as u32, end as u32)
        })
    }

    fn next_useful(&mut self) -> Option<Token<'a>> {
        let token = self.next()?;

        if token.kind == TokenKind::Comment || token.kind == TokenKind::Whitespace {
            self.next_useful()
        } else {
            Some(token)
        }
    }
}