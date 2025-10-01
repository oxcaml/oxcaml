# js_of_ocaml Integration Design Document

## Executive Summary

This document describes the integration of js_of_ocaml into the OxCaml compiler, enabling JavaScript as a first-class compilation target alongside native and bytecode. The integration leverages OxCaml's Flambda2 optimizer to generate optimized JavaScript code, potentially exceeding the performance of standard js_of_ocaml compilation.

## Goals and Motivation

### Primary Goals
1. **Enable JavaScript compilation from OxCaml** - Support web deployment of OCaml applications compiled with Jane Street's OxCaml
2. **Leverage Flambda2 optimizations** - Apply OxCaml's advanced optimizations before JavaScript code generation
3. **Maintain ecosystem compatibility** - Preserve compatibility with existing js_of_ocaml tools and libraries
4. **Treat JavaScript as first-class backend** - Full integration into build system, testing, and tooling

### Why This Approach?
- **Vendor-and-modify over forking**: Reduces maintenance burden and allows tracking upstream js_of_ocaml updates
- **JSIR as bridge**: Reuses js_of_ocaml's mature JavaScript code generation while benefiting from Flambda2
- **Unified optimization pipeline**: Single optimization pass (Flambda2) for all backends

## Architecture Overview

### Compilation Pipeline

**Native/Bytecode Path (existing):**
```
Source → Lambda → Flambda2 → CMM → CFG -> Machine Code
```

**JavaScript Path (new):**
```
Source → Lambda → Flambda2 → JSIR → js_of_ocaml → JavaScript
```

### Key Components

#### 1. The `ocamlj` Compiler Driver
- **Location**: `driver/jscompile.ml`, `driver/jsmaindriver.ml`
- **Purpose**: JavaScript counterpart to `ocamlopt`
- **Output files**:
  - `.cmjx` - JavaScript compilation metadata (identical format to `.cmx` but for 32-bit JavaScript)
  - `.cmjxa` - JavaScript archives (identical format to `.cmxa` but for 32-bit JavaScript)
  - `.cmjo` - JavaScript object files (counterpart to native `.o` files)
  - `.cmja` - JavaScript library archives (counterpart to native `.a` files)
  - `.js` - Final JavaScript executables and user-written stubs

**Note**: Separate extensions are necessary to distinguish compiler output from user-written `.js` files.

**Design Decision**: Mirror the `ocamlopt` interface for consistency and ease of adoption.

#### 2. JSIR (JavaScript Intermediate Representation)
- **Location**: `middle_end/flambda2/to_jsir/`
- **Purpose**: Bridge between Flambda2's optimized representation and js_of_ocaml's IR
- **Key insight**: Generate js_of_ocaml's internal IR directly from Flambda2, bypassing js_of_ocaml's own optimization passes

**Why JSIR?**
- Allows leveraging js_of_ocaml's mature JavaScript code generation
- Enables Flambda2 optimizations that js_of_ocaml doesn't perform, such as flattening unboxed types
- Maintains compatibility with js_of_ocaml runtime and ecosystem
- Reduces our dependency on the bytecode compiler/instruction set
  - This also reduces our need to add new C stubs for every single operation that we add.

#### 3. JavaScript Compilation Subsystem (`jscomp/`)
- **jslibrarian**: Creates JavaScript library archives (`.cmjxa` and `.cmja`)
- **jslink**: Links JavaScript executables, manages runtime

**Design Decision**: Moved jslibrarian from `driver/` to `jscomp/` to create a dedicated JavaScript subsystem, separating concerns from the main compiler driver.

## Implementation Details

### Flambda2 Integration

The JavaScript backend configures Flambda2 with:
- **Machine width**: `Thirty_two_no_gc_tag_bit` (JavaScript's 32-bit integer constraints)
- **No GC tag bits**: JavaScript doesn't use OCaml's tagged integer representation

```ocaml
Flambda2.lambda_to_flambda ~machine_width:Thirty_two_no_gc_tag_bit
  ~ppf_dump:i.ppf_dump
  ~prefixname:(Unit_info.prefix i.target)
  { program with code }
```

### Primitive Translation

**Location**: `to_jsir_primitive.ml`

Maps Flambda2 primitives to JavaScript operations, handling:
- Different integer bit widths (tagged, int32, int64, nativeint)
- Float32/Float64 distinctions
- Array operations with JavaScript semantics
- External function calls

**Example pattern**:
```ocaml
let with_int_prefix_exn ~(kind : Flambda_kind.Standard_int.t) ~percent_for_imms op =
  match kind, percent_for_imms with
  | (Tagged_immediate | Naked_immediate), true -> "%int"
  | (Tagged_immediate | Naked_immediate), false -> "caml_int"
  | Naked_int32, _ -> "caml_int32"
  | Naked_int64, _ -> "caml_int64"
```

### Runtime Strategy

The JavaScript linker (`jslink.ml`) implements the js_of_ocaml runtime building strategy:

1. Build runtime file: `output.js.runtime.js` containing the js_of_ocaml runtime and any additional JavaScript primitives
2. Link runtime with compiled libraries/objects
3. Clean up temporary runtime file


### Build System Integration

#### Dune Configuration
- New library: `ocamljcomp` containing JavaScript compilation components
- New executable: `boot_ocamlj` for bootstrapping
- Dynamic rule generation for standard library compilation (`tools/gen_ocamlj_rules.sh` )

#### Parallel Build Infrastructure
Maintains parallel file extensions for each backend:
- Native: `.cmx` → `.cmxa` → `.opt` (or platform executable)
- Bytecode: `.cmo` → `.cma` → `.byte`
- JavaScript: `.cmjx` → `.cmjxa` → `.js`

### Testing Infrastructure

JavaScript is a first-class backend in the test system:

```ocaml
type t = Native | Bytecode | Javascript

let module_extension = make_backend_function "cmo" "cmx" "cmjx"
let library_extension = make_backend_function "cma" "cmxa" "cmjxa"
let executable_extension = make_backend_function "byte" "opt" "js"
```

**Key features**:
- JavaScript-specific test references
- Stubs for JavaScript-incompatible C bindings
- Size metrics collection for JavaScript artifacts

## Areas Requiring Clarification

### 1. Source Map Support
**Current Status**: Flambda2's offsets and reachable names are currently ignored during JSIR translation. This means source map support is not yet implemented.

**Impact**: Debugging compiled JavaScript code maps back to generated JavaScript rather than original OCaml source.

### 2. Effects Runtime Configuration
**Location**: `--enable=effects,with-js-error`

**Design Decision**: Effects are always enabled for simplicity. This could be made configurable in future work but is not a current priority.

### 3. Float32 Support
**Implementation**: JavaScript only has Float64, so Float32 operations are emulated:
- Each operation is performed in 64-bit precision then rounded using `Math.fround()`
- **Important**: Marshalled float32 data is incompatible between JavaScript and native programs

From `external/js_of_ocaml/runtime/js/float32.js`:
```javascript
// 32-bit floats are represented as javascript numbers, i.e. 64-bit floats.
// Each operation is performed in 64-bit precision and then rounded to the
// nearest 32-bit float.
function caml_float32_of_float(x) {
    return Math.fround(x);
}
```

### 4. Domain Operations
**Implementation**: Domains are executed synchronously in JavaScript's single-threaded environment:
- `caml_domain_spawn` immediately executes the domain body and returns
- No true parallelism - domains run sequentially
- Maintains API compatibility but without concurrency benefits
- Domain-local storage (DLS) and atomic operations are implemented but operate in a single-threaded context


## Performance Considerations

### Optimization Strategy
1. **Flambda2 first**: All high-level optimizations happen in Flambda2
2. **JSIR translation**: Direct translation preserves optimizations
3. **js_of_ocaml code gen**: Only low-level JavaScript generation

**Expected benefits**:
- Better inlining decisions (Flambda2 has more context)
- Cross-module optimizations
- Consistent optimization across backends

**Potential concerns**:
- Translation overhead
- Lost js_of_ocaml-specific optimizations
- Debugging complexity

## Migration Path

### For OxCaml Users
1. Use `ocamlj` instead of `ocamlopt` for JavaScript targets
2. Link with `ocamlj -o output.js` instead of js_of_ocaml link
3. Existing js_of_ocaml libraries should work unchanged

### For js_of_ocaml Users
- This is an alternative path, not a replacement
- Standard js_of_ocaml still works for non-OxCaml code
- Benefits mainly for Flambda2-heavy optimizations

## Future Work

### Potential Improvements
1. **Source maps**: Implement support using Flambda2 offset information (currently ignored)
2. **Tree shaking**: More aggressive dead code elimination
3. **Wasm backend**: Leverage JSIR for WebAssembly
4. **Streaming compilation**: Faster build times
5. **JavaScript-specific optimizations**: Leverage JavaScript engine quirks
6. **Configurable effects**: Make effects support optional for performance-sensitive code
7. **True parallelism**: Explore Web Workers for domain support

### Open Questions
1. Should we upstream JSIR support to js_of_ocaml?
2. Can we share more code with js_of_ocaml?

## Appendix: File Format Reference

| Extension | Purpose | Counterpart | Notes |
|-----------|---------|-------------|-------|
| `.cmjx` | JavaScript compilation unit interface | `.cmx` | Same format, 32-bit compilation |
| `.cmjxa` | JavaScript library interface | `.cmxa` | Same format, 32-bit compilation |
| `.cmjo` | JavaScript object file | `.o` | Contains generated JavaScript code |
| `.cmja` | JavaScript library archive | `.a` | Collection of JavaScript objects |
| `.js` | JavaScript executable/stub | - | Final executables or user stubs |

## Appendix: Key Source Locations

- `driver/jscompile.ml` - Main compilation driver
- `driver/jsmaindriver.ml` - Command-line interface
- `middle_end/flambda2/to_jsir/` - JSIR translation
- `jscomp/` - JavaScript-specific compilation tools
- `external/js_of_ocaml/` - Modified js_of_ocaml vendor
- `testsuite/tests/` - Test modifications
- `ocamltest/ocaml_backends.ml` - Test backend support
