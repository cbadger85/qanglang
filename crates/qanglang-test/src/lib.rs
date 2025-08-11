use std::{collections::HashMap, fs};

use qanglang_core::{
    CompilerPipeline, FunctionValueKind, HeapObject, ObjectHandle, ObjectHeap, SourceMap, Value, Vm,
};

pub fn run_tests(files: Vec<std::path::PathBuf>, vm_loader: fn(&mut Vm) -> ()) {
    let mut results = Vec::new();

    for file in files {
        results.push(run_test(&file.to_string_lossy(), vm_loader));
    }

    let fatal_count = results.iter().filter(|result| result.is_fatal()).count();
    let total_errors: usize = results
        .iter()
        .map(|result| match result {
            TestSuiteResult::Success {
                name,
                results,
                description,
            } => results.iter().filter(|r| r.is_failure()).count(),
            _ => 0,
        })
        .sum();
    let total_passed: usize = results
        .iter()
        .map(|result| match result {
            TestSuiteResult::Success {
                name,
                results,
                description,
            } => results.iter().filter(|r| r.is_success()).count(),
            _ => 0,
        })
        .sum();

    for result in &results {
        match result {
            TestSuiteResult::Success {
                name,
                results,
                description,
            } => {
                if let Some(desc) = description {
                    println!("{} - {}", name, desc);
                } else {
                    println!("{}", name);
                }
                for test_result in results {
                    if test_result.is_success() {
                        println!("  ✓ {}", test_result.name);
                    } else {
                        println!("  ✗ {}", test_result.name);
                        if let Some(reason) = &test_result.failure_reason {
                            println!("    {}", reason);
                        }
                    }
                }
            }
            TestSuiteResult::Failure { name, reason } => {
                println!("{}", name);
                println!("  ✗ Suite failed: {}", name);
                println!("    {}", reason);
            }
        }
    }

    println!();
    if fatal_count > 0 {
        println!("Fatal errors: {}", fatal_count);
    }
    if total_errors > 0 {
        println!("Failed tests: {}", total_errors);
    }
    println!("Passed tests: {}", total_passed);

    let total_tests = total_passed + total_errors;
    if total_tests > 0 {
        println!("Total: {} tests", total_tests);
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct TestDetails {
    description: Option<String>,
    test_handles: Vec<(String, ObjectHandle)>,
}

pub enum TestSuiteResult {
    Success {
        name: String,
        results: Vec<TestResult>,
        description: Option<String>,
    },
    Failure {
        name: String,
        reason: String,
    },
}

impl TestSuiteResult {
    fn is_fatal(&self) -> bool {
        matches!(self, Self::Failure { name: _, reason: _ })
    }
}

pub struct TestResult {
    name: String,
    failure_reason: Option<String>,
}

impl TestResult {
    fn success(name: String) -> Self {
        Self {
            name,
            failure_reason: None,
        }
    }

    fn failure(name: String, failure_resason: String) -> Self {
        Self {
            name,
            failure_reason: Some(failure_resason),
        }
    }

    fn is_success(&self) -> bool {
        self.failure_reason.is_none()
    }

    fn is_failure(&self) -> bool {
        self.failure_reason.is_some()
    }
}

pub fn run_test(filename: &str, vm_loader: fn(&mut Vm) -> ()) -> TestSuiteResult {
    // let filename = file.to_string_lossy();
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            let filename = filename.to_string();
            let reason = format!("Error reading file '{}': {}", &filename, err);
            return TestSuiteResult::Failure {
                name: filename,
                reason,
            };
        }
    };
    let source_map = SourceMap::new(source);
    let mut heap = ObjectHeap::new();

    let mut vm = match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let mut vm = Vm::new(heap);
            match vm.interpret(program) {
                Ok(_) => vm,
                Err(error) => {
                    return TestSuiteResult::Failure {
                        name: filename.to_string(),
                        reason: error.message,
                    };
                }
            }
        }
        Err(error) => {
            for error in error.all() {
                eprintln!("{}", error);
            }

            let reason: String = error
                .into_errors()
                .into_iter()
                .map(|e| e.message)
                .collect::<Vec<String>>()
                .join("\n");

            return TestSuiteResult::Failure {
                name: filename.to_string(),
                reason,
            };
        }
    };

    vm_loader(&mut vm);

    let test_details = extract_test_details(vm.globals(), vm.heap());

    let mut test_results = Vec::new();

    for (function_name, function_handle) in test_details.test_handles {
        match vm.call_function_with_args(function_handle, Vec::new()) {
            Ok(_) => {
                test_results.push(TestResult::success(function_name));
            }
            Err(error) => {
                test_results.push(TestResult::failure(function_name, error.message));
            }
        }
    }

    TestSuiteResult::Success {
        name: filename.to_string(),
        results: test_results,
        description: test_details.description,
    }
}

fn extract_test_details(globals: &HashMap<usize, Value>, heap: &ObjectHeap) -> TestDetails {
    let mut details: Vec<(Box<str>, Value)> = Vec::new();

    for (handle_id, value) in globals.iter() {
        let handle = ObjectHandle::new(*handle_id);

        let identifier = heap
            .get(handle)
            .and_then(|obj| match obj {
                HeapObject::String(identifier) => Some(identifier),
                _ => None,
            })
            .filter(|identifier| identifier.starts_with("test_"));

        if let Some(identifier) = identifier {
            details.push((identifier.clone(), *value))
        }
        if identifier.is_some() {}
    }

    let name = details.iter().find_map(|(_, value)| match value {
        Value::String(handle) => match heap.get(*handle) {
            Some(HeapObject::String(description)) => Some(description.to_string()),
            _ => None,
        },
        _ => None,
    });

    let test_handles = details
        .iter()
        .filter_map(|(identifier_name, value)| match value {
            Value::Function(FunctionValueKind::QangFunction(handle)) => {
                Some((identifier_name.to_string(), *handle))
            }
            _ => None,
        })
        .collect();

    TestDetails {
        description: name,
        test_handles,
    }
}
