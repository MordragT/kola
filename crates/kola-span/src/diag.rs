use camino::Utf8Path;
use derive_more::Display;
use kola_utils::interner_ext::{DisplayWithInterner, WithInterner};
use owo_colors::{OwoColorize, Style};
use std::{
    fmt,
    io::{self, Write},
};

use crate::{Loc, Located, SourceManager};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReportCheckpoint {
    pub issues: usize,
    pub diagnostics: usize,
}

/// Represents a report containing multiple issues and diagnostics.
#[derive(Debug, Clone, Default)]
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

    /// Creates a checkpoint of the current state of the report, allowing you to reset to this point later.
    pub fn checkpoint(&self) -> ReportCheckpoint {
        ReportCheckpoint {
            issues: self.issues.len(),
            diagnostics: self.diagnostics.len(),
        }
    }

    /// Resets the report to a previous checkpoint, removing any issues or diagnostics added after the checkpoint.
    pub fn reset(&mut self, checkpoint: ReportCheckpoint) {
        self.issues.truncate(checkpoint.issues);
        self.diagnostics.truncate(checkpoint.diagnostics);
    }

    /// Adds an issue to the report.
    pub fn add_issue(&mut self, issue: Issue) {
        self.issues.push(issue);
    }

    /// Adds a diagnostic to the report.
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Adds multiple issues to the report.
    pub fn extend_issues(&mut self, issues: impl IntoIterator<Item = Issue>) {
        self.issues.extend(issues);
    }

    /// Adds multiple diagnostics to the report.
    pub fn extend_diagnostics(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    /// Checks if the report contains any issues or diagnostics.
    pub fn is_empty(&self) -> bool {
        self.issues.is_empty() && self.diagnostics.is_empty()
    }

    /// Writes the report to the specified writer.
    pub fn write(self, mut w: impl Write, cache: &SourceManager) -> io::Result<()> {
        for issue in self.issues {
            issue.write(&mut w)?;
        }
        for diagnostic in self.diagnostics {
            diagnostic.write(&mut w, cache)?;
        }
        Ok(())
    }

    /// Prints the report to the standard output.
    pub fn print(self, cache: &SourceManager) -> io::Result<()> {
        self.write(io::stdout(), cache)
    }

    /// Prints the report to the standard error output.
    pub fn eprint(self, cache: &SourceManager) -> io::Result<()> {
        self.write(io::stderr(), cache)
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

    pub fn write(&self, mut w: impl Write) -> io::Result<()> {
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

    pub fn print(&self) -> io::Result<()> {
        self.write(io::stdout())
    }

    pub fn eprint(&self) -> io::Result<()> {
        self.write(io::stderr())
    }
}

impl fmt::Display for Issue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expected<T> {
    Tokens(Vec<T>),
    EndOfFile,
    Anything,
    Custom(String),
}

impl<T> fmt::Display for Expected<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Tokens(tokens) => {
                for (i, token) in tokens.iter().enumerate() {
                    if i > 0 {
                        if i == tokens.len() - 1 {
                            write!(f, " or ")?;
                        } else {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "`{token}`")?;
                }
                Ok(())
            }
            Expected::EndOfFile => write!(f, "end of file"),
            Expected::Anything => write!(f, "anything"),
            Expected::Custom(msg) => msg.fmt(f),
        }
    }
}

impl<T> From<String> for Expected<T> {
    fn from(value: String) -> Self {
        Expected::Custom(value)
    }
}

impl<T> From<&str> for Expected<T> {
    fn from(value: &str) -> Self {
        Expected::Custom(value.to_owned())
    }
}

impl<T> From<Vec<T>> for Expected<T> {
    fn from(value: Vec<T>) -> Self {
        Expected::Tokens(value)
    }
}

impl<T, const N: usize> From<[T; N]> for Expected<T> {
    fn from(value: [T; N]) -> Self {
        Expected::Tokens(value.into_iter().collect())
    }
}

impl<T> Expected<T> {
    pub fn tokens(tokens: impl IntoIterator<Item = T>) -> Self {
        Self::Tokens(tokens.into_iter().collect())
    }

    pub fn eof() -> Self {
        Self::EndOfFile
    }

    pub fn anything() -> Self {
        Self::Anything
    }

    pub fn custom(msg: impl Into<String>) -> Self {
        Self::Custom(msg.into())
    }

    pub fn merge(self, other: Self) -> Self
    where
        T: fmt::Display,
    {
        match (self, other) {
            (Expected::Tokens(mut a), Expected::Tokens(b)) => {
                a.extend(b);
                Expected::Tokens(a)
            }
            (Expected::Anything, _) | (_, Expected::Anything) => Expected::Anything,
            (Expected::EndOfFile, Expected::EndOfFile) => Expected::EndOfFile,
            (Expected::Custom(a), Expected::Custom(b)) if a == b => Expected::Custom(a),
            (a, b) => Expected::Custom(format!("{} or {}", a, b)),
        }
    }
}

/// Represents an expected value that was not found during processing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Miss<T> {
    pub loc: Loc,
    pub expected: Expected<T>,
    pub found: Option<T>,
}

impl<T> fmt::Display for Miss<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let found = match &self.found {
            Some(t) => format!("`{t}`"),
            None => "end of file".to_string(),
        };

        write!(f, "expected `{}`, found `{}`", self.expected, found)
    }
}

impl<T> Miss<T> {
    /// Token was present but wrong — "expected X, found Y"
    pub fn new(loc: Loc, expected: impl Into<Expected<T>>, found: T) -> Self {
        Self {
            loc,
            expected: expected.into(),
            found: Some(found),
        }
    }

    /// Input ended unexpectedly — "expected X, found end of file"
    pub fn eof(loc: Loc, expected: impl Into<Expected<T>>) -> Self
    where
        T: ToString,
    {
        Self {
            loc,
            expected: expected.into(),
            found: None,
        }
    }

    /// Merge another Expected into self — combines alternatives,
    /// keeping self's loc and found.
    pub fn merge(mut self, other: Miss<T>) -> Self
    where
        T: fmt::Display,
    {
        self.expected = self.expected.merge(other.expected);
        self
    }

    /// Convert this Miss into a Diagnostic, promoting it to an error.
    pub fn throw(self) -> Diagnostic
    where
        T: fmt::Display,
    {
        Diagnostic::error(self.loc, self.to_string())
    }
}

// /// Represents an unexpected end of input during processing, along with the expected values.
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct EndOfFile<T> {
//     pub loc: Loc,
//     pub expected: Expected<T>,
// }

// impl<T> fmt::Display for EndOfFile<T>
// where
//     T: fmt::Display,
// {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "expected `{}`, found end of file", self.expected)
//     }
// }

// impl<T> EndOfFile<T> {
//     /// Input ended unexpectedly — "expected X, found end of file"
//     pub fn new(loc: Loc, expected: impl Into<Expected<T>>) -> Self {
//         Self {
//             loc,
//             expected: expected.into(),
//         }
//     }

//     /// Convert this EndOfFile into a Diagnostic, promoting it to an error.
//     pub fn throw(self) -> Diagnostic
//     where
//         T: fmt::Display,
//     {
//         Diagnostic::error(self.loc, self.to_string())
//     }
// }

/// A parse failure — either a speculative miss (backtrackable) or a
/// committed error (non-backtrackable, shown to the user).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Failure<T> {
    // /// Speculative failure — input ended unexpectedly, backtracking is
    // /// allowed. Never shown to the user directly.
    // EndOfFile(EndOfFile<T>),
    /// Speculative failure — no input consumed at the failure point,
    /// backtracking is allowed. Never shown to the user directly.
    Miss(Miss<T>),
    /// Committed failure — past a commitment point, backtracking is
    /// forbidden. Added to the Report and shown to the user.
    Raise(Diagnostic),
}

impl<T> Failure<T> {
    /// Speculative miss — wrong token present
    pub fn miss(loc: Loc, expected: impl Into<Expected<T>>, found: T) -> Self {
        Self::Miss(Miss::new(loc, expected, found))
    }

    /// Speculative miss — input ended unexpectedly
    pub fn eof(loc: Loc, expected: impl Into<Expected<T>>) -> Self
    where
        T: ToString,
    {
        Self::Miss(Miss::eof(loc, expected))
        // Self::EndOfFile(EndOfFile::new(loc, expected))
    }

    /// Committed error — wraps a full diagnostic
    pub fn raise(diagnostic: Diagnostic) -> Self {
        Self::Raise(diagnostic)
    }

    // /// Whether this failure is an unexpected end of input.
    // #[inline]
    // pub fn is_eof(&self) -> bool {
    //     matches!(self, Self::EndOfFile(_))
    // }

    /// Whether this failure is a speculative miss.
    #[inline]
    pub fn is_miss(&self) -> bool {
        matches!(self, Self::Miss(_))
    }

    /// Whether this failure is a committed error.
    #[inline]
    pub fn is_raise(&self) -> bool {
        matches!(self, Self::Raise(_))
    }

    /// Promote a `Miss` to a `Commit` — use at commitment points
    /// (e.g. after an open delimiter has been consumed).
    #[inline]
    pub fn promote(self) -> Self
    where
        T: fmt::Display,
    {
        match self {
            // Self::EndOfFile(e) => Self::Raise(e.throw()),
            Self::Miss(e) => Self::Raise(e.throw()),
            _ => self,
        }
    }

    /// Extract the diagnostic, promoting a `Miss` to a `Raise` if necessary.
    #[inline]
    pub fn throw(self) -> Diagnostic
    where
        T: fmt::Display,
    {
        match self {
            // Self::EndOfFile(e) => e.throw(),
            Self::Miss(e) => e.throw(),
            Self::Raise(d) => d,
        }
    }

    /// Extract the diagnostic if committed, or `None` if a miss.
    #[inline]
    pub fn diagnostic(self) -> Option<Diagnostic> {
        match self {
            Self::Raise(d) => Some(d),
            _ => None,
        }
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

impl<T> IntoDiagnostic for WithInterner<'_, T, str> where T: DisplayWithInterner<str> {}
impl<T> IntoDiagnostic for WithInterner<'_, T, Utf8Path> where T: DisplayWithInterner<Utf8Path> {}

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

    /// Adds a single trace element to the diagnostic and returns self for method chaining.
    ///
    /// # Arguments
    /// * `label` - The trace label
    /// * `loc` - The source location associated with the trace
    pub fn with_trace_element(mut self, label: impl Into<String>, loc: Loc) -> Self {
        self.trace.push((label.into(), loc));
        self
    }

    /// Adds a single trace element to the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `label` - The trace label
    /// * `loc` - The source location associated with the trace
    pub fn add_trace_element(&mut self, label: impl Into<String>, loc: Loc) -> &mut Self {
        self.trace.push((label.into(), loc));
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

    /// Adds a single note to the diagnostic and returns self for method chaining.
    ///
    /// # Arguments
    /// * `note` - The note to add
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Adds a single note to the diagnostic and returns a mutable reference for further modification.
    ///
    /// # Arguments
    /// * `note` - The note to add
    pub fn add_note(&mut self, note: impl Into<String>) -> &mut Self {
        self.notes.push(note.into());
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

    /// Writes the diagnostic to the specified writer.
    pub fn write(self, w: impl Write, cache: &SourceManager) -> io::Result<()> {
        let report = ariadne::Report::from(self);
        report.write(cache, w)
    }

    /// Prints the diagnostic to the standard output.
    pub fn print(self, cache: &SourceManager) -> io::Result<()> {
        self.write(io::stdout(), cache)
    }

    /// Prints the diagnostic to the standard error output.
    pub fn eprint(self, cache: &SourceManager) -> io::Result<()> {
        self.write(io::stderr(), cache)
    }
}

impl From<Diagnostic> for ariadne::Report<'_, Loc> {
    fn from(diag: Diagnostic) -> Self {
        let Diagnostic {
            message,
            help,
            severity,
            loc,
            mut trace,
            notes,
        } = diag;

        // TODO why the hell must I do this ??
        // This is a workaround for weird behavior in ariadne,
        // where it doesnt render if the trace is empty.
        trace.push((message, loc));

        let mut builder = ariadne::Report::build(severity.into(), loc)
            // .with_message(message)
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
