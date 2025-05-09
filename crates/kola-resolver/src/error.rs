use std::hash::Hash;

use kola_syntax::prelude::*;
use kola_vfs::{
    diag::{SourceDiagnostic, SourceReport},
    source::Source,
};
use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Module Report:")]
pub struct ModuleReport {
    #[label = "In this module"]
    pub span: Option<Span>,
    #[source_code]
    pub src: Source,
    #[related]
    pub related: Vec<SourceDiagnostic>,
}

impl ModuleReport {
    pub fn new(report: SourceReport, span: Option<Span>) -> Self {
        Self {
            span,
            src: report.src,
            related: report.related,
        }
    }
}

impl From<SourceReport> for ModuleReport {
    fn from(report: SourceReport) -> Self {
        Self {
            span: None,
            src: report.src,
            related: report.related,
        }
    }
}

#[derive(Error, Debug, Default, Diagnostic)]
#[error("Module Reports:")]
pub struct ModuleReports(#[related] Vec<ModuleReport>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameCollision {
    ValueBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
    ModuleBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
    TypeBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
}

impl NameCollision {
    pub fn value_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::ValueBind { span, bind, help }
    }
    pub fn module_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::ModuleBind { span, bind, help }
    }
    pub fn type_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::TypeBind { span, bind, help }
    }
}

impl From<NameCollision> for SourceDiagnostic {
    fn from(value: NameCollision) -> Self {
        match value {
            NameCollision::ValueBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A value bind with the same name was defined before")
                    .with_trace([("This value bind here".to_owned(), bind)])
                    .with_help(help)
            }
            NameCollision::ModuleBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A module bind with the same name was defined before")
                    .with_trace([("This module bind here".to_owned(), bind)])
                    .with_help(help)
            }
            NameCollision::TypeBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A type bind with the same name was defined before")
                    .with_trace([("This type bind here".to_owned(), bind)])
                    .with_help(help)
            }
        }
    }
}
