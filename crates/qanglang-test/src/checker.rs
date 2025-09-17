use std::sync::Arc;

use qanglang_core::{
    AnalysisPipeline, AnalysisPipelineConfig, ErrorMessageFormat, Parser, ParserConfig, SourceMap,
    StringInterner, TypedNodeArena,
};

use crate::test_file::SourceFile;

#[derive(Debug, Clone)]
pub struct CheckResult {
    pub display_path: String,
    pub passed: bool,
    pub error_count: usize,
    pub errors: Vec<String>,
}

impl CheckResult {
    pub fn success(display_path: String) -> Self {
        Self {
            display_path,
            passed: true,
            error_count: 0,
            errors: Vec::new(),
        }
    }

    pub fn failure(display_path: String, errors: Vec<String>) -> Self {
        let error_count = errors.len();
        Self {
            display_path,
            passed: false,
            error_count,
            errors,
        }
    }
}

pub fn check_files_from_sources(
    source_files: Vec<SourceFile>,
    error_format: ErrorMessageFormat,
) -> Vec<CheckResult> {
    source_files
        .into_iter()
        .map(|source_file| check_single_file(source_file, error_format))
        .collect()
}

pub fn check_single_file(
    source_file: SourceFile,
    error_message_format: ErrorMessageFormat,
) -> CheckResult {
    let source_map = match SourceMap::from_path(&source_file.file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!(
                "Error reading file '{}': {}",
                source_file.file_path.display(),
                err
            );
            std::process::exit(1);
        }
    };
    let source_map = Arc::new(source_map);
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();
    let mut parser = Parser::new(source_map, &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let modules = parser.parse();

    let mut errors = parser.into_errors();
    let analyzer = AnalysisPipeline::new(&mut strings).with_config(AnalysisPipelineConfig {
        error_message_format,
        ..Default::default()
    });

    // Try to compile the file
    match analyzer.analyze(&modules, &mut nodes, &mut errors) {
        Ok(_) => CheckResult::success(source_file.display_path),
        Err(compilation_errors) => {
            let error_messages: Vec<String> = compilation_errors
                .into_errors()
                .into_iter()
                .map(|e| e.message)
                .collect();

            CheckResult::failure(source_file.display_path, error_messages)
        }
    }
}

pub fn format_check_results(results: &[CheckResult]) -> String {
    let mut output = String::new();

    // Print each file result
    for result in results {
        if result.passed {
            output.push_str(&format!("✓ {}\n", result.display_path));
        } else {
            output.push_str(&format!(
                "✗ {} ({} error{})\n",
                result.display_path,
                result.error_count,
                if result.error_count == 1 { "" } else { "s" }
            ));

            // Print the errors for this file
            for error in &result.errors {
                output.push_str(&format!("  {}\n", error));
            }
        }
    }

    // Summary statistics
    let total_files = results.len();
    let failed_files = results.iter().filter(|r| !r.passed).count();
    let passed_files = total_files - failed_files;
    let total_errors: usize = results.iter().map(|r| r.error_count).sum();

    output.push('\n');
    if failed_files > 0 {
        output.push_str(&format!(
            "Failed: {} file{}\n",
            failed_files,
            if failed_files == 1 { "" } else { "s" }
        ));
        output.push_str(&format!("Total errors: {}\n", total_errors));
    }
    output.push_str(&format!(
        "Passed: {} file{}\n",
        passed_files,
        if passed_files == 1 { "" } else { "s" }
    ));
    output.push_str(&format!(
        "Total: {} file{}\n",
        total_files,
        if total_files == 1 { "" } else { "s" }
    ));

    output
}
