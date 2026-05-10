use crate::{Diagnostic, Report, diag::ReportCheckpoint, input::Input};

/// The outcome of a failed parse attempt.
///
/// Both variants cause the parser to backtrack. The difference is whether
/// the failure has been explicitly thrown:
///
/// - [`Failure::Abort`] — the default failure kind. Never touches the [`Report`].
/// - [`Failure::Emit`] — produced by the [`throw`] combinator. The diagnostic
///   has been added to the [`Report`] and can be retrieved by an enclosing [`catch`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Failure {
    /// A failure explicitly marked by [`throw`].
    ///
    /// The diagnostic has already been added to the [`Report`] at the carried
    /// index. An enclosing [`catch`] will drain the report from that point
    /// and pass the diagnostics to its fallback.
    Emit(ReportCheckpoint),

    /// The default failure — the parser did not match the current input.
    ///
    /// Never touches the [`Report`]. Silently discarded on backtrack.
    Abort(Diagnostic),
}

impl Failure {
    pub fn extract(self, report: &mut Report, report_cp: ReportCheckpoint) -> Diagnostic {
        match self {
            Self::Abort(diag) => {
                report.reset(report_cp);
                diag
            }
            Self::Emit(cp) => {
                debug_assert!(
                    cp >= report_cp,
                    "Emit checkpoint predates report checkpoint"
                );
                report
                    .split_reset(report_cp)
                    .flatten()
                    .expect("Emit always has diagnostics in the report.")
            }
        }
    }
}

impl From<Diagnostic> for Failure {
    fn from(diag: Diagnostic) -> Self {
        Failure::Abort(diag)
    }
}

impl From<ReportCheckpoint> for Failure {
    fn from(cp: ReportCheckpoint) -> Self {
        Failure::Emit(cp)
    }
}

pub trait Parser<I: Input, O>: Sized {
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure>;
}

impl<I, O, P> Parser<I, O> for &P
where
    I: Input,
    P: Parser<I, O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        (**self).parse(input, report)
    }
}

pub trait IterParser<I: Input, O>: Sized {
    type State: Default;

    /// Drive one step of the iteration.
    /// Returns:
    ///   Ok(Some(item)) - parsed an item, continue
    ///   Ok(None)       - done, stop iteration
    ///   Err(diag)      - fatal error
    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic>;
}

impl<I, O, P> IterParser<I, O> for &P
where
    I: Input,
    P: IterParser<I, O>,
{
    type State = P::State;

    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        (**self).drive(state, input, report)
    }
}
