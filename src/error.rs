use crate::{SourceMap, ast::SourceSpan, chunk::SourceLocation};

#[derive(Debug, Clone, PartialEq)]
pub struct QangSyntaxError {
    pub message: String,
    pub span: SourceSpan,
}

impl QangSyntaxError {
    pub fn new(message: String, span: SourceSpan) -> Self {
        Self { message, span }
    }

    pub fn new_formatted(
        message: &str,
        span: SourceSpan,
        source_map: &SourceMap,
    ) -> QangSyntaxError {
        let message = pretty_print_syntax_error(source_map, message, span);
        QangSyntaxError::new(message, span)
    }
}

impl std::fmt::Display for QangSyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for QangSyntaxError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Trace {
    callee: Box<str>,
    loc: SourceLocation,
}

impl Trace {
    pub fn new(callee: Box<str>, loc: SourceLocation) -> Self {
        Self { callee, loc }
    }
}

impl std::fmt::Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  at {} (line {}, column {})", self.callee, self.loc.line, self.loc.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct QangRuntimeError {
    pub message: String,
    pub stack_trace: Vec<Trace>,
}

impl QangRuntimeError {
    pub fn new(message: String, loc: SourceLocation) -> Self {
        Self {
            message: format!(
                "Runtime Error at line {}, column {}: {}",
                loc.line, loc.col, message
            )
            .to_string(),
            stack_trace: Vec::new(),
        }
    }

    pub fn new_with_trace(message: String, loc: SourceLocation, stack_trace: Vec<Trace>) -> Self {
        Self {
            message: format!(
                "Runtime Error at line {}, column {}: {}",
                loc.line, loc.col, message
            )
            .to_string(),
            stack_trace,
        }
    }

    pub fn with_stack_trace(mut self, stack_trace: Vec<Trace>) -> Self {
        self.stack_trace = stack_trace;
        self
    }
}

impl std::fmt::Display for QangRuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if !self.stack_trace.is_empty() {
            writeln!(f)?;
            writeln!(f, "Stack trace:")?;
            for trace in &self.stack_trace {
                writeln!(f, "{}", trace)?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for QangRuntimeError {}

#[derive(Debug, Clone)]
pub struct ValueConversionError(String);

impl ValueConversionError {
    pub fn new(message: &str) -> Self {
        Self(message.to_string())
    }

    pub fn into_qang_error_with_trace(
        self,
        loc: SourceLocation,
        stack_trace: Vec<Trace>,
    ) -> QangRuntimeError {
        QangRuntimeError::new_with_trace(self.0, loc, stack_trace)
    }

    pub fn into_qang_error(self, loc: SourceLocation) -> QangRuntimeError {
        QangRuntimeError::new(self.0, loc)
    }
}

/// Handles error reporting and pretty printing for the QangLang compiler.
#[derive(Debug, Clone, Default)]
pub struct ErrorReporter {
    errors: Vec<QangSyntaxError>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Add an error to the error list
    pub fn report_error(&mut self, error: QangSyntaxError) {
        self.errors.push(error);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn errors(&self) -> &[QangSyntaxError] {
        &self.errors
    }

    pub fn take_errors(self) -> Vec<QangSyntaxError> {
        self.errors
    }
}

/// Pretty print a single error with source context
pub fn pretty_print_syntax_error(
    source_map: &SourceMap,
    message: &str,
    span: SourceSpan,
) -> String {
    let mut output = String::new();

    let line_num = source_map.get_line_number(span.start);
    let col_num = source_map.get_column_number(span.start);

    // Error header with kind
    output.push_str(&format!(
        "{} at line {}, column {}: {}\n",
        "Syntax Error", line_num, col_num, message
    ));

    // Get the problematic line
    let line_chars = source_map.get_line(line_num);
    let line_str: String = line_chars.iter().collect();

    // Line number padding for alignment
    let line_num_str = line_num.to_string();
    let padding = " ".repeat(line_num_str.len());

    // Show the line with line number
    output.push_str(&format!(" {} | {}\n", line_num, line_str));

    // Create the error pointer
    let error_pointer = create_error_pointer(source_map, span, &line_str, col_num);
    output.push_str(&format!(" {} | {}\n", padding, error_pointer));

    output
}

/// Create a visual pointer to the error location
fn create_error_pointer(
    source_map: &SourceMap,
    span: SourceSpan,
    line_str: &str,
    col_num: u32,
) -> String {
    let mut pointer = String::new();

    // Add spaces up to the error column
    for i in 1..col_num {
        if i <= line_str.len() as u32 {
            let ch = line_str.chars().nth((i - 1) as usize).unwrap_or(' ');
            if ch == '\t' {
                pointer.push('\t');
            } else {
                pointer.push(' ');
            }
        } else {
            pointer.push(' ');
        }
    }

    // Calculate the span length within the line
    let start_col = source_map.get_column_number(span.start) as usize;
    let end_col = source_map.get_column_number(span.end.saturating_sub(1)) as usize;

    // Add the error indicators
    if start_col == end_col || span.start == span.end {
        pointer.push('^');
    } else {
        // Multi-character span
        let span_length = (end_col - start_col + 1).max(1);
        for i in 0..span_length {
            if i == 0 {
                pointer.push('^');
            } else {
                pointer.push('~');
            }
        }
    }

    pointer
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceMap;

    fn error_summary(errors: &[QangSyntaxError]) -> String {
        if errors.is_empty() {
            "No errors found.".to_string()
        } else if errors.len() == 1 {
            "Found 1 error.".to_string()
        } else {
            format!("Found {} errors.", errors.len())
        }
    }

    #[test]
    fn test_error_reporter_basic_functionality() {
        let source = "var x = 5 +\nvar y = 10;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new();

        // Report an error
        reporter.report_error(QangSyntaxError::new_formatted(
            "Expected expression after '+'",
            SourceSpan::new(11, 11),
            &source_map,
        ));

        assert!(reporter.has_errors());
        assert_eq!(reporter.errors().len(), 1);
    }

    #[test]
    fn test_multiple_errors() {
        let source = "var x = 5 +\nvar y = 10 *\nvar z;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new();

        // Report multiple errors
        reporter.report_error(QangSyntaxError::new_formatted(
            "Missing operand after '+'",
            SourceSpan::new(11, 11),
            &source_map,
        ));

        reporter.report_error(QangSyntaxError::new_formatted(
            "Missing operand after '*'",
            SourceSpan::new(23, 23),
            &source_map,
        ));

        assert_eq!(reporter.errors().len(), 2);

        let summary = error_summary(reporter.errors());
        assert_eq!(summary, "Found 2 errors.");
    }

    #[test]
    fn test_error_summary_no_errors() {
        let reporter = ErrorReporter::new();

        let summary = error_summary(reporter.errors());
        assert!(summary.contains("No errors found."));
    }

    #[test]
    fn test_error_summary_single_errors() {
        let source = "var x = 5;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new();

        reporter.report_error(QangSyntaxError::new_formatted(
            "Syntax error 1",
            SourceSpan::new(0, 3),
            &source_map,
        ));

        let summary = error_summary(reporter.errors());
        assert!(summary.contains("Found 1 error."));
    }

    #[test]
    fn test_error_summary_multiple_errors() {
        let source = "var x = 5;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new();

        reporter.report_error(QangSyntaxError::new_formatted(
            "Syntax error 1",
            SourceSpan::new(0, 3),
            &source_map,
        ));
        reporter.report_error(QangSyntaxError::new_formatted(
            "Syntax error 2",
            SourceSpan::new(4, 5),
            &source_map,
        ));

        let summary = error_summary(reporter.errors());
        assert!(summary.contains("Found 2 errors."));
    }
}
