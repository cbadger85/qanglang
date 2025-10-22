# QangLang

A scripting language written in Rust 🦀

## Installation

```bash
cargo install qanglang-cli
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

```ql
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

## VSCode Extension

There is a published VSCode extension that provides syntax and error highlighting.

- [QangLang in the marketplace](https://marketplace.visualstudio.com/items?itemName=cbadger85.qanglang)

## Development

### Running Tests

The project tests can be ran by running the following command:

```sh
cargo test
```

### Running Language Tests

While developing, it's often good to check against the language tests to make sure nothing is broken. Run the following command to run the language tests.

```sh
cargo run -- test tests
```

When checking against release, the following command can be used:

```sh
qang tests test
```
