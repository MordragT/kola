use chumsky::error::Rich;
use derive_more::Display;
use owo_colors::{OwoColorize, Style};
use std::{
    fmt,
    io::{self, Write},
};

use crate::{Loc, Located, SourceCache};

/// Represents a report containing multiple issues and diagnostics.
pub struct Report {
    pub issues: Vec<Issue>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Report {
    /// Creates a new report with empty issues and diagnostics.
    pub fn new() -> Self {
        Self {
            issues: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Adds an issue to the report.
    pub fn add_issue(&mut self, issue: Issue) {
        self.issues.push(issue);
    }

    /// Adds a diagnostic to the report.
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Prints the report to the standard output.
    pub fn print(self, cache: &SourceCache) -> io::Result<()> {
        for issue in self.issues {
            issue.print()?;
        }
        for diagnostic in self.diagnostics {
            diagnostic.print(cache)?;
        }
        Ok(())
    }

    /// Prints the report to the standard error output.
    pub fn eprint(self, cache: &SourceCache) -> io::Result<()> {
        for issue in self.issues {
            issue.eprint()?;
        }
        for diagnostic in self.diagnostics {
            diagnostic.eprint(cache)?;
        }
        Ok(())
    }
}

//// Represents the severity of a diagnostic message.
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Info,
    Warning,
    Error,
}

impl From<Severity> for ariadne::ReportKind<'_> {
    fn from(value: Severity) -> Self {
        match value {
            Severity::Info => ariadne::ReportKind::Advice,
            Severity::Warning => ariadne::ReportKind::Warning,
            Severity::Error => ariadne::ReportKind::Error,
        }
    }
}

/// Converts a type into an issue.
pub trait IntoIssue: fmt::Display + Sized {
    /// Converts the type into an issue.
    fn into_issue(self, code: usize) -> Issue {
        Issue::error(self.to_string(), code)
    }
}

impl IntoIssue for io::Error {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Issue {
    /// The main issue message.
    pub message: String,
    /// Code associated with the issue.
    pub code: usize,
    /// Optional help text that provides additional guidance.
    pub help: Option<String>,
    /// The severity of the issue (Error, Warning, or Advice).
    pub severity: Severity,
}

impl Issue {
    /// Creates a new issue with the specified message, code, and severity.
    pub fn error(message: impl Into<String>, code: usize) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Error,
            code,
        }
    }

    /// Creates a new issue with the specified message, code, and severity.
    pub fn warn(message: impl Into<String>, code: usize) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Warning,
            code,
        }
    }

    /// Creates a new issue with the specified message, code, and severity.
    pub fn info(message: impl Into<String>, code: usize) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Info,
            code,
        }
    }

    /// Adds help text to the issue and returns self for method chaining.
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Sets help text on the issue and returns a mutable reference for further modification.
    pub fn set_help(&mut self, help: impl Into<String>) -> &mut Self {
        self.help = Some(help.into());
        self
    }

    pub fn write(self, mut w: impl Write) -> io::Result<()> {
        let Issue {
            message,
            code,
            help,
            severity,
        } = self;

        let style = match severity {
            Severity::Info => Style::new().green(),
            Severity::Warning => Style::new().yellow(),
            Severity::Error => Style::new().red(),
        };

        writeln!(
            w,
            "{} {}: {message}",
            code.style(style),
            severity.style(style)
        )?;

        if let Some(help) = help {
            writeln!(w, "{} {help}", "Help:".cyan())?;
        }

        Ok(())
    }

    pub fn print(self) -> io::Result<()> {
        self.write(io::stdout())
    }

    pub fn eprint(self) -> io::Result<()> {
        self.write(io::stderr())
    }
}

impl fmt::Display for Issue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

/// Converts a type into a source diagnostic.
pub trait IntoDiagnostic: fmt::Display + Sized {
    /// Converts the type into a source diagnostic.
    fn into_diagnostic(self, loc: Loc) -> Diagnostic {
        Diagnostic::error(loc, self.to_string())
    }
}

impl IntoDiagnostic for io::Error {}

/// Represents a diagnostic message with source location information.
///
/// Source diagnostics provide information about errors, warnings, or informational
/// messages related to source code. They include a message, optional help text,
/// a severity level, a location in the source code (span), and optional trace
/// information for additional context.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    /// The main diagnostic message.
    pub message: String,
    /// Optional help text that provides additional guidance.
    pub help: Option<String>,
    /// The severity of the diagnostic (Error, Warning, or Advice).
    pub severity: Severity,
    /// The source code location this diagnostic refers to.
    pub loc: Loc,
    /// Additional context information with their respective locations.
    pub trace: Vec<Located<String>>,
    /// Additional notes related to the diagnostic.
    pub notes: Vec<String>,
}

impl Diagnostic {
    /// Creates a new error diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the error refers to
    /// * `message` - The error message
    pub fn error(loc: Loc, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Error,
            loc,
            trace: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Creates a new warning diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the warning refers to
    /// * `message` - The warning message
    pub fn warn(loc: Loc, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Warning,
            loc,
            trace: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Creates a new informational diagnostic.
    ///
    /// # Arguments
    /// * `span` - The source location the info refers to
    /// * `message` - The informational message
    pub fn info(loc: Loc, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            severity: Severity::Info,
            loc,
            trace: Vec::new(),
            notes: Vec::new(),
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
    pub fn with_trace(mut self, trace: impl IntoIterator<Item = Located<String>>) -> Self {
        self.trace = trace.into_iter().collect();
        self
    }

    /// Sets trace information on the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `trace` - Collection of trace elements with their source spans
    pub fn set_trace(&mut self, trace: impl IntoIterator<Item = Located<String>>) -> &mut Self {
        self.trace = trace.into_iter().collect();
        self
    }

    /// Adds notes to the diagnostic and returns self for method chaining.
    ///
    /// # Arguments
    /// * `notes` - Collection of notes to add
    pub fn with_notes(mut self, notes: impl IntoIterator<Item = String>) -> Self {
        self.notes = notes.into_iter().collect();
        self
    }

    /// Sets notes on the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `notes` - Collection of notes to set
    pub fn set_notes(&mut self, notes: impl IntoIterator<Item = String>) -> &mut Self {
        self.notes = notes.into_iter().collect();
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
        self.severity == Severity::Info
    }

    /// Prints the diagnostic to the standard output.
    pub fn print(self, cache: &SourceCache) -> io::Result<()> {
        let report = ariadne::Report::from(self);
        report.print(cache)
    }

    /// Prints the diagnostic to the standard error output.
    pub fn eprint(self, cache: &SourceCache) -> io::Result<()> {
        let report = ariadne::Report::from(self);
        report.eprint(cache)
    }
}

/// Converts a Chumsky parser error into a source diagnostic.
impl<'t, T> From<Rich<'t, T, Loc>> for Diagnostic
where
    T: fmt::Display,
{
    fn from(e: Rich<'t, T, Loc>) -> Self {
        let message = e.to_string();
        let loc = *e.span();
        let trace = e
            .contexts()
            .map(|(label, span)| (label.to_string(), *span))
            .collect();

        Self {
            message,
            help: None,
            severity: Severity::Error,
            loc,
            trace,
            notes: Vec::new(),
        }
    }
}

impl From<Diagnostic> for ariadne::Report<'_, Loc> {
    fn from(diag: Diagnostic) -> Self {
        let Diagnostic {
            message,
            help,
            severity,
            loc,
            trace,
            notes,
        } = diag;

        let mut builder = ariadne::Report::build(severity.into(), loc)
            .with_message(message)
            .with_labels(
                trace
                    .into_iter()
                    .map(|(label, loc)| ariadne::Label::new(loc).with_message(label)),
            );

        builder.with_helps(help);
        builder.with_notes(notes);

        builder.finish()
    }
}

/// Implements display formatting for source diagnostics.
impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

/// Implements the Error trait for SourceDiagnostic.
impl std::error::Error for Diagnostic {}
