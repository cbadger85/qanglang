# QangLang

A scripting language written in Rust 🦀

## Installation

```bash
cargo build --release
```

## Usage

REPL:

```bash
qang
```

Run a script:

```bash
qang run script.ql
```

Run tests:

```bash
qang test test_script.ql
```

Check syntax:

```bash
qang check script.ql
```

## Example

```QangLang
fn greet(name) {
  return "Hello, " + name;
}

var message = greet("World");
println(message);
```

## Project Structure

- `qanglang-core` - Language core and VM
- `qanglang-cli` - Command-line interface
- `qanglang-ls` - Language server
- `qanglang-test` - Test runner
