# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains the Flow programming language - a language designed specifically for optimal interaction with Large Language Models (LLMs). The project is currently in the specification/planning phase.

## Current Status

The repository currently contains only the language specification (`FLOW_LANGUAGE_SPEC.md`). No implementation exists yet.

## Language Design Goals

Flow prioritizes:
- **Explicit intent declaration** - Every function declares its purpose clearly
- **Self-documenting syntax** - Natural language-like code
- **Built-in testing** - Tests are part of function definitions
- **Universal tool interface** - Call functions from Python, JavaScript, Rust, etc.
- **Checkpoints** - Allow partial execution and testing during code generation

## Implementation Roadmap

When implementing Flow, follow this phased approach:

### Phase 1: Core Language
- Parser for basic syntax
- Type system implementation
- Basic function definitions
- Pattern matching (when/otherwise)

### Phase 2: Testing Framework
- Inline test execution
- Property-based testing
- Example-driven development
- Checkpoint system

### Phase 3: Interoperability
- FFI design
- Python integration
- JavaScript integration
- Type conversion system

### Phase 4: Advanced Features
- Context management
- Error recovery strategies
- Tool discovery
- Semantic types

### Phase 5: Tooling
- LSP implementation
- REPL
- Debugger with checkpoint support
- Documentation generator

## Key Language Features to Implement

1. **Function Structure**:
   ```flow
   intent: <description>
   accepts: <input types>
   returns: <output type>
   tests: <test cases>
   function name(params): <implementation>
   ```

2. **External Tool Integration**:
   ```flow
   external tool from python: <module>
   ```

3. **Checkpoints**:
   ```flow
   checkpoint after: "description" yields variable
   ```

4. **Error Handling**:
   ```flow
   attempt: <code>
   recover with <strategy>: <recovery code>
   ```

## Target Platforms

- **Primary**: WebAssembly (for universal deployment)
- **Secondary**: Native compilation via LLVM
- **Transpilation**: JavaScript, Python, Go

## Design Decisions Needed

Before implementing, consider:
1. Gradual vs strict typing
2. Async/concurrent operation syntax
3. Complex data transformation syntax
4. Built-in observability/monitoring
5. External tool versioning

## Development Setup

Once implementation begins, you'll need to:
1. Choose implementation language (consider Rust for WASM target)
2. Set up parser generator or hand-write parser
3. Create build system
4. Establish testing framework

## File Organization (Suggested)

When starting implementation:
```
/src
  /parser     - Lexer and parser implementation
  /types      - Type system
  /runtime    - Runtime and execution engine
  /ffi        - Foreign function interface
  /testing    - Built-in testing framework
/examples     - Flow code examples
/tests        - Compiler/runtime tests
```