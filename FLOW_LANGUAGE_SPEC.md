# Flow: An LLM-Optimized Programming Language

## Overview

Flow is a programming language designed specifically for optimal interaction with Large Language Models (LLMs). Unlike traditional languages designed for human programmers, Flow prioritizes clarity, explicitness, and verifiability - traits that make it ideal for LLM code generation, understanding, and manipulation.

## Core Design Principles

### 1. Explicit Intent Declaration
- Every function/module starts with a clear intent statement
- Purpose is explicit and machine-readable
- No ambiguous function names or unclear purposes

### 2. Self-Documenting Syntax
- Code reads like natural language where possible
- No cryptic operators or symbols
- Clear, descriptive keywords

### 3. Immutable by Default
- Reduces side effects and state tracking complexity
- Makes code easier to reason about
- Explicit when mutation is needed

### 4. Built-in Testing
- Test cases are part of the function definition
- Automatic property-based testing generation
- Examples drive implementation

### 5. Universal Tool Interface
- Standardized way to call functions from any language
- Declarative FFI (Foreign Function Interface)
- Automatic type conversions at boundaries

## Language Syntax

### Basic Function Structure

```flow
intent: Calculate fibonacci number with memoization
accepts: Integer n where n >= 0
returns: Integer representing the nth fibonacci number
tests:
  fibonacci(0) yields 0
  fibonacci(1) yields 1
  fibonacci(5) yields 5
  fibonacci(10) yields 55
  
function fibonacci(n):
  when n is 0: return 0
  when n is 1: return 1
  otherwise: return fibonacci(n-1) + fibonacci(n-2)
  with cache
```

### External Tool Integration

```flow
external tool from python: numpy.array
external tool from rust: regex.Regex  
external tool from javascript: lodash.debounce

intent: Process data using multiple language tools
accepts: List of numbers
returns: Processed result

function processData(numbers):
  checkpoint after: "Matrix creation" yields matrix
  let matrix = numpy.array(numbers as PythonList).reshape(-1, 2)
    expecting: "2D array with shape (n, 2)"
    on error: "Input list length must be even for reshape to (n, 2)"
  
  checkpoint after: "Filtering" yields filtered
  let filtered = matrix 
    as JavaScriptArray
    |> filter with javascript: """x => x > 10"""
    as FlowList
    
  return filtered
```

### Error Handling

```flow
function processRequest(request):
  attempt:
    let parsed = JSON.parse(request.body)
  recover with default:
    return HTTPResponse(400, "Invalid JSON: ${error.message}")
  recover with retry(3):
    let parsed = JSON.parse(cleanupJSON(request.body))
  finally fail:
    log error to monitoring
    return HTTPResponse(500, "Unable to process request")
```

### Type System

```flow
type Email = String matching: """^[^\s@]+@[^\s@]+\.[^\s@]+$"""
  with meaning: "Valid email address"
  
type Price = Decimal where value >= 0
  with unit: "USD"
  with precision: 2

type Product = {
  title: NonEmptyString,
  price: Price with currency: "USD", 
  image: URL,
  available: Boolean
}
```

### Context Management

```flow
context WebScraping:
  timeout: 30 seconds
  retry: 3 times
  user_agent: "Mozilla/5.0..."
  
within context WebScraping:
  function fetchPage(url):
    // Automatically uses context settings
```

### Example-Driven Development

```flow
intent: Calculate age from birthdate
examples:
  calculateAge("1990-01-01", "2024-01-01") yields 34
  calculateAge("2024-01-01", "2024-01-01") yields 0
  calculateAge("2024-01-01", "2023-01-01") fails with "Future birthdate"

function calculateAge(birthdate, currentDate):
  infer implementation from examples
```

## Key Features for LLM Optimization

### 1. Checkpoints
Allow partial execution and testing during code generation:
```flow
checkpoint after: "Input validation"
checkpoint after: "Data transformation" yields transformedData
```

### 2. Explicit Type Conversions
Clear boundaries between language interop:
```flow
let jsArray = pythonList as JavaScriptArray
let flowList = jsArray as FlowList
```

### 3. Inline Documentation Queries
```flow
explain: "Why this threshold?"
let filtered = data |> filter where value > 0.7
```

### 4. Automatic Tool Discovery
```flow
intent: Convert markdown to PDF
discover tools for: "markdown to PDF conversion"
```

### 5. Property-Based Testing
```flow
properties:
  for all valid_urls: no exceptions thrown
  for all results: each product has required fields
```

## Complete Example: Web Scraper

```flow
intent: Robust web scraper with error handling
examples:
  scrapeProducts("https://shop.example.com") yields products where length > 0
  scrapeProducts("https://invalid.url") recovers gracefully

external tool from python: beautifulsoup4.BeautifulSoup
external tool from javascript: puppeteer

context ProductScraping:
  timeout: 30 seconds
  retry: 3 times
  
type Product = {
  title: NonEmptyString,
  price: Price with currency: "USD",
  image: URL,
  available: Boolean
}

within context ProductScraping:
  function scrapeProducts(url: URL, selectors: Map<String, CSSSelector>):
    checkpoint after: "Page load"
    attempt:
      let page = puppeteer.launch() |> navigate to url
    recover with retry(context.retry):
      wait 5 seconds
      let page = puppeteer.launch() |> navigate to url
      
    checkpoint after: "Content extraction" yields products
    let products = page.content()
      as String
      |> BeautifulSoup(_, "html.parser")
      |> extract products where:
          title: _.select_one(selectors.title)?.text or skip
          price: parsePrice(_.select_one(selectors.price)?.text) or skip
          image: _.select_one(selectors.image)?["src"] or placeholder
          available: _.select_one(selectors.stock) exists
      
    ensuring:
      page?.close() regardless of outcome
      
    return products where valid
```

## Implementation Plan

### Phase 1: Core Language
1. Parser for basic syntax
2. Type system implementation
3. Basic function definitions
4. Pattern matching (when/otherwise)

### Phase 2: Testing Framework
1. Inline test execution
2. Property-based testing
3. Example-driven development
4. Checkpoint system

### Phase 3: Interoperability
1. FFI design
2. Python integration
3. JavaScript integration
4. Type conversion system

### Phase 4: Advanced Features
1. Context management
2. Error recovery strategies
3. Tool discovery
4. Semantic types

### Phase 5: Tooling
1. LSP (Language Server Protocol) implementation
2. REPL
3. Debugger with checkpoint support
4. Documentation generator

## Target Platforms

- **Primary:** WebAssembly (for universal deployment)
- **Secondary:** Native compilation via LLVM
- **Transpilation targets:** JavaScript, Python, Go

## Why This Works for LLMs

1. **Predictable patterns** - Consistent syntax following natural language flow
2. **Explicit contracts** - Clear intent, inputs, outputs, and tests
3. **No hidden state** - Immutability and explicit effects
4. **Rich error context** - Detailed error messages and recovery strategies
5. **Incremental validation** - Checkpoints allow testing during generation
6. **Self-verifying** - Built-in tests enable automatic verification

## Open Questions

1. Should we support gradual typing or enforce strict typing?
2. How to handle async/concurrent operations?
3. What's the best syntax for complex data transformations?
4. Should we include built-in observability/monitoring?
5. How to handle versioning of external tools?

## Next Steps

1. Create formal grammar specification
2. Build proof-of-concept parser
3. Implement basic type checker
4. Create simple transpiler to JavaScript
5. Test with real LLM code generation scenarios