use qanglang_core::{
    GlobalCompilerPipeline, ErrorMessageFormat,
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
    // Use GlobalCompilerPipeline to check multiple files efficiently
    // by parsing them all together (better module resolution)
    let file_paths: Vec<_> = source_files.iter().map(|sf| sf.file_path.clone()).collect();

    let mut pipeline = GlobalCompilerPipeline::new();
    let mut allocator = qanglang_core::HeapAllocator::new();

    // Parse all files at once for better dependency resolution
    match pipeline.process_files(file_paths.clone(), &mut allocator) {
        Ok(_) => {
            // All files passed - create success results
            source_files
                .into_iter()
                .map(|source_file| CheckResult::success(source_file.display_path))
                .collect()
        }
        Err(_) => {
            // Some files failed - check each individually to get specific errors
            source_files
                .into_iter()
                .map(|source_file| check_single_file(source_file, error_format))
                .collect()
        }
    }
}

pub fn check_single_file(
    source_file: SourceFile,
    _error_message_format: ErrorMessageFormat,
) -> CheckResult {
    // Use the new GlobalCompilerPipeline for better module handling
    let mut pipeline = GlobalCompilerPipeline::new();
    let mut allocator = qanglang_core::HeapAllocator::new();

    // Parse files and automatically determine main modules
    match pipeline.parse_files_auto_main(vec![source_file.file_path.clone()], &mut allocator) {
        Ok(_) => {}
        Err(parse_error) => {
            let error_messages: Vec<String> = parse_error
                .into_errors()
                .into_iter()
                .map(|e| e.message)
                .collect();
            return CheckResult::failure(source_file.display_path, error_messages);
        }
    }

    // Run semantic analysis on all parsed modules
    match pipeline.process_files(vec![source_file.file_path], &mut allocator) {
        Ok(_) => CheckResult::success(source_file.display_path),
        Err(analysis_error) => {
            let error_messages: Vec<String> = analysis_error
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
