# Flow Language

An LLM-optimized programming language designed for clarity, explicitness, and seamless interaction with Large Language Models.

## Features

- **Explicit Intent Declaration** - Every function clearly states its purpose
- **Built-in Testing** - Tests are part of the language syntax
- **Universal Tool Interface** - Call functions from Python, JavaScript, Rust, and more
- **Checkpoints** - Partial execution and debugging support
- **Natural Language Syntax** - Readable and writable by both humans and LLMs

## Installation

### Prerequisites

1. Install Rust (1.70 or later):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Add WebAssembly target (for WASM builds):
   ```bash
   rustup target add wasm32-unknown-unknown
   ```

3. Install wasm-pack (optional, for WASM builds):
   ```bash
   curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
   ```

### Building from Source

```bash
# Clone the repository
git clone https://github.com/AmbientWare/flow-lang.git
cd flow-lang

# Build the project
cargo build --release

# Run tests
cargo test

# Install the Flow CLI
cargo install --path .
```

## Usage

### Run a Flow Program
```bash
flow run example.flow
```

### Start the REPL
```bash
flow repl
# or just
flow
```

### Check Syntax and Types
```bash
flow check example.flow
```

### Run Tests
```bash
flow test example.flow
```

## Example

```flow
intent: Calculate factorial with memoization
accepts: Integer n where n >= 0
returns: Integer
tests:
  factorial(0) yields 1
  factorial(5) yields 120
  factorial(10) yields 3628800

function factorial(n):
  when n is 0: return 1
  when n is 1: return 1
  otherwise: return n * factorial(n - 1)
  with cache
```

## Development

### Project Structure

```
flow/
├── src/
│   ├── lexer/        # Tokenization
│   ├── parser/       # AST generation
│   ├── types/        # Type system
│   ├── runtime/      # Runtime environment
│   ├── interpreter/  # Code execution
│   ├── ffi/          # Foreign function interface
│   └── testing/      # Built-in test framework
├── flow-cli/         # Command-line interface
├── flow-lsp/         # Language server protocol
├── flow-wasm/        # WebAssembly bindings
├── examples/         # Example Flow programs
└── tests/            # Compiler tests
```

### Running Development Commands

```bash
# Run with logging
RUST_LOG=debug cargo run

# Run specific tests
cargo test lexer::tests

# Check code style
cargo fmt -- --check
cargo clippy -- -D warnings

# Build documentation
cargo doc --open

# Benchmark performance
cargo bench
```

### Contributing

1. Check `IMPLEMENTATION_TASKS.md` for current tasks
2. Read `FLOW_LANGUAGE_SPEC.md` for language design
3. Follow Rust standard style guidelines
4. Add tests for new features
5. Update documentation as needed

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

at your option.

## Status

Flow is in early development. See `IMPLEMENTATION_TASKS.md` for progress.