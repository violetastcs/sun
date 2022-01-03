use std::collections::HashMap;

use codespan::Span;

use crate::parser::{CompilationUnit, Type, ItemPath, Statement, StatementKind};

#[derive(Debug, Clone)]
pub struct Function {
    args: Vec<String>,
    span: Span
}

#[derive(Debug, Clone)]
pub struct Decl {
    kind: Type,
    span: Span
}

#[derive(Debug, Clone)]
pub struct UnitInfo {
    decls: HashMap<String, Decl>,
    functions: HashMap<String, Function>
}

pub fn analyze_unit(unit: &CompilationUnit) -> UnitInfo {
    let mut functions = HashMap::new();
    let mut decls = HashMap::new();

    for statement in &unit.statements {
        match statement {
            Statement { kind: StatementKind::Defun {
                name, args, ..
            }, span } => {
                functions.insert(name.clone(), Function {
                    args: args.clone(),
                    span: span.clone()
                });
            },

            Statement { kind: StatementKind::Decl {
                name, kind
            }, span } => {
                decls.insert(name.clone(), Decl {
                    kind: kind.clone(),
                    span: span.clone()
                });
            }

            _ => ()
        }
    }

    UnitInfo {
        functions: functions,
        decls: decls
    }
}