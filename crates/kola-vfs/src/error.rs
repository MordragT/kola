use chumsky::error::Rich;
use kola_syntax::{
    span::{Span, Spanned},
    token::Token,
};
use miette::{Diagnostic, LabeledSpan, Severity};
use std::fmt;
use thiserror::Error;

use crate::source::Source;

/// A type alias for results that may contain source diagnostics.
///
/// This is typically used for operations that can produce errors during parsing,
/// typechecking, or other source code analysis operations.
pub type SourceResult<T> = Result<T, Vec<SourceDiagnostic>>;

#[derive(Error, Debug, Diagnostic)]
#[error("Reporting Source Diagnostics:")]
pub struct SourceReport {
    #[source_code]
    pub src: Source,
    #[related]
    pub related: Vec<SourceDiagnostic>,
}

impl SourceReport {
    pub fn new(source: Source, related: Vec<SourceDiagnostic>) -> Self {
        Self {
            src: source,
            related,
        }
    }
}

/// Represents a diagnostic message with source location information.
///
/// Source diagnostics provide information about errors, warnings, or informational
/// messages related to source code. They include a message, optional help text,
/// a severity level, a location in the source code (span), and optional trace
/// information for additional context.
#[derive(Debug, Clone)]
pub struct SourceDiagnostic {
    /// The main diagnostic message.
    pub message: String,
    /// Optional help text that provides additional guidance.
    pub help: Option<String>,
    /// The severity of the diagnostic (Error, Warning, or Advice).
    pub severity: Severity,
    /// The source code location this diagnostic refers to.
    pub span: Span,
    /// Additional context information with their respective locations.
    pub trace: Vec<Spanned<String>>,
}

impl SourceDiagnostic {
    /// Creates a new error diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the error refers to
    /// * `message` - The error message
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Error,
            span,
            trace: Vec::new(),
        }
    }

    /// Creates a new warning diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the warning refers to
    /// * `message` - The warning message
    pub fn warn(span: Span, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Warning,
            span,
            trace: Vec::new(),
        }
    }

    /// Creates a new informational diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the info refers to
    /// * `message` - The informational message
    pub fn info(span: Span, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Advice,
            span,
            trace: Vec::new(),
        }
    }

    /// Adds help text to the diagnostic and returns self for method chaining.
    ///
    /// # Arguments
    /// * `help` - The help text to add
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Sets help text on the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `help` - The help text to set
    pub fn set_help(&mut self, help: impl Into<String>) -> &mut Self {
        self.help = Some(help.into());
        self
    }

    /// Adds trace information to the diagnostic and returns self for method chaining.
    ///
    /// # Arguments
    /// * `trace` - Collection of trace elements with their source spans
    pub fn with_trace(mut self, trace: impl IntoIterator<Item = Spanned<String>>) -> Self {
        self.trace = trace.into_iter().collect();
        self
    }

    /// Sets trace information on the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `trace` - Collection of trace elements with their source spans
    pub fn set_trace(&mut self, trace: impl IntoIterator<Item = Spanned<String>>) -> &mut Self {
        self.trace = trace.into_iter().collect();
        self
    }

    /// Checks if this diagnostic is an error.
    ///
    /// # Returns
    /// `true` if the diagnostic is an error, `false` otherwise
    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    /// Checks if this diagnostic is a warning.
    ///
    /// # Returns
    /// `true` if the diagnostic is a warning, `false` otherwise
    pub fn is_warning(&self) -> bool {
        self.severity == Severity::Warning
    }

    /// Checks if this diagnostic is an informational message.
    ///
    /// # Returns
    /// `true` if the diagnostic is informational, `false` otherwise
    pub fn is_info(&self) -> bool {
        self.severity == Severity::Advice
    }

    pub fn report(self, source: Source) -> SourceReport {
        SourceReport::new(source, vec![self])
    }
}

/// Converts a Chumsky lexer error into a source diagnostic.
impl<'t> From<Rich<'t, char>> for SourceDiagnostic {
    fn from(e: Rich<'t, char>) -> Self {
        let message = e.to_string();
        let span = *e.span();
        let trace = e
            .contexts()
            .map(|(label, span)| (label.to_string(), *span))
            .collect();

        Self {
            message,
            help: None,
            severity: Severity::Error,
            span,
            trace,
        }
    }
}

/// Converts a Chumsky parser error into a source diagnostic.
impl<'t> From<Rich<'t, Token<'t>, Span>> for SourceDiagnostic {
    fn from(e: Rich<'t, Token<'t>, Span>) -> Self {
        let message = e.to_string();
        let span = *e.span();
        let trace = e
            .contexts()
            .map(|(label, span)| (label.to_string(), *span))
            .collect();

        Self {
            message,
            help: None,
            severity: Severity::Error,
            span,
            trace,
        }
    }
}

/// Implements display formatting for source diagnostics.
impl fmt::Display for SourceDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

/// Implements the Error trait for SourceDiagnostic.
impl std::error::Error for SourceDiagnostic {}

/// Implements the miette Diagnostic trait to enable rich error reporting.
impl Diagnostic for SourceDiagnostic {
    /// Returns the severity of this diagnostic.
    fn severity(&self) -> Option<Severity> {
        Some(self.severity)
    }

    /// Returns the help text for this diagnostic, if any.
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.help
            .as_ref()
            .map(|advice| Box::new(advice) as Box<dyn fmt::Display>)
    }

    /// Returns the labeled spans for this diagnostic.
    ///
    /// This includes the primary span with the main message and any trace spans.
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let primary =
            LabeledSpan::new_primary_with_span(Some(self.message.clone()), self.span.into_range());

        let trace = self.trace.iter().map(|(label, span)| {
            LabeledSpan::new_with_span(Some(label.clone()), span.into_range())
        });

        let iter = Box::new(std::iter::once(primary).chain(trace))
            as Box<dyn Iterator<Item = LabeledSpan>>;

        Some(iter)
    }
}
