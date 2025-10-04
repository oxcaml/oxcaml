# js_of_ocaml Integration Design Document

## Executive Summary

This document describes the integration of js_of_ocaml into the OxCaml compiler, enabling JavaScript as a first-class compilation target alongside native and bytecode. The integration exposes the backend through `ocamlopt -target js_of_ocaml` and leverages OxCaml's Flambda2 optimizer to generate optimized JavaScript code.

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

#### 1. The JavaScript Backend (`ocamlopt -target js_of_ocaml`)
- **Location**: `optcomp/jscomp.ml`, `driver/optmaindriver.ml`, `driver/compenv.ml`
- **Purpose**: JavaScript counterpart to `ocamlopt`, activated via `-target js_of_ocaml`
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

#### 3. JavaScript Compilation Helpers (`optcomp/jscomp.ml`)
- Wrap the shared `Optcompile` pipeline, translate Flambda2 output to JSIR, and delegate to the vendor `js_of_ocaml` CLI for compilation, archive creation, and linking.
- Resolve inputs through `Load_path` so `.cmjo`, `.cmjxa`, and raw `.js` support files can be reused by the linker without bespoke tooling.

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

`optcomp/jscomp.ml` invokes the js_of_ocaml CLI directly:

1. `js_of_ocaml build-runtime` produces `output.runtime.js`, combining the js_of_ocaml runtime with any `.js` helper files gathered from `Clflags.ccobjs`.
2. `js_of_ocaml link` then stitches together the runtime, generated `.cmjo` units, and other JavaScript objects.
3. The temporary runtime file is removed once linking completes.


### Build System Integration

#### Build Tooling
- JavaScript compilation logic lives alongside the native backend in `ocamloptcomp` (`optcomp/jscomp.ml`, driver hooks, runtime helpers).
- `driver/compenv.ml` recognises `.js` stubs when the JavaScript backend is active so they are forwarded to the linker just like `.c` files for native builds.

#### Parallel Build Infrastructure
Maintains parallel file extensions for each backend:
- Native: `.cmx` → `.cmxa` → `.opt` (or platform executable)
- Bytecode: `.cmo` → `.cma` → `.byte`
- JavaScript: `.cmjx` → `.cmjxa` → `.js`

### Testing Infrastructure

JavaScript is a first-class backend in ocamltest:

- `ocamltest/ocaml_compilers.ml` defines a compiler descriptor that always applies `-target js_of_ocaml` when the JavaScript backend is requested.
- `ocamltest/ocaml_actions.ml` filters backend-specific sources so `.c` stubs are ignored and `.js` helpers are retained for JavaScript runs.
- Built-in predicates skip tests that rely on unsupported libraries and effect handlers. All suites under `testsuite/tests/effects/` are automatically marked as skipped, avoiding noisy failures until the runtime grows real effect support.

## Areas Requiring Clarification

### 1. Source Map Support
**Current Status**: Flambda2's offsets and reachable names are currently ignored during JSIR translation. This means source map support is not yet implemented.

**Impact**: Debugging compiled JavaScript code maps back to generated JavaScript rather than original OCaml source.

### 2. Effects Runtime Configuration
**Location**: `--enable=effects,with-js-error`

**Design Decision**: Effects are always enabled for simplicity. This could be made configurable in future work but is not a current priority.

### 3. Float32 Support
JavaScript only has Float64, so Float32 operations are emulated (see `external/js_of_ocaml/runtime/js/float32.js`). Each operation is performed in 64-bit precision then rounded using `Math.fround()`. Marshalled float32 data is therefore incompatible between JavaScript and native programs.

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
1. Use `ocamlopt -target js_of_ocaml` for JavaScript targets
2. Link with `ocamlopt -target js_of_ocaml -o output.js` instead of js_of_ocaml link
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
6. **Effects runtime**: Implement real effect handlers instead of skipping the test suite
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

- `optcomp/jscomp.ml` - JavaScript backend implementation
- `driver/optmaindriver.ml` / `driver/compenv.ml` - Driver entry and argument handling
- `middle_end/flambda2/to_jsir/` - JSIR translation
- `external/js_of_ocaml/` - Modified js_of_ocaml vendor
- `testsuite/tests/` - Test modifications
- `ocamltest/ocaml_compilers.ml`, `ocamltest/ocaml_actions.ml`, `ocamltest/builtin_actions.ml` - Test harness integration

## Implementation Notes (2025)

### ocamlopt Driver and Backend Wiring
- The `Optcomp.Make` functor in `optcomp/optcompile.ml` provides the shared compilation pipeline. Each backend implements `Optcomp_intf.Backend`, defining extensions, link/emit hooks, and `compile_implementation`. The native backend wraps `Asmgen`, while the JavaScript backend lives in `optcomp/jscomp.ml` and delegates to `js_of_ocaml`.
- `driver/optmaindriver.ml` chooses a backend by inspecting `Clflags.backend_target ()`. The new `-target` flag wires to `Clflags.set_backend_target` during argument parsing, letting the driver instantiate `Jscomp.Make(Flambda2)` when `-target js_of_ocaml` is supplied.
- `driver/compenv.ml` drives command processing. It now special-cases `.js` files when the active backend is JavaScript so C-style foreign stubs packaged as raw JavaScript are added to `ccobjs` and forwarded to the js linker. The same module also handles default output naming; when `-o` is omitted, `ocamlopt -target js_of_ocaml` defaults to `a.out.js`.

### JavaScript Backend Specifics
- `optcomp/jscomp.ml` is the glue layer between the shared `Optcompile` pipeline and the vendor `js_of_ocaml` CLI. It:
  - Reuses a helper `link_args` to construct `js_of_ocaml` command lines and centralises handling of `--debug-info`, `--linkall`, and `Clflags.all_ccopts`.
  - Builds the runtime with `js_of_ocaml build-runtime`, then links compiled objects (resolved through `Load_path`) along with any `Clflags.ccobjs` carried over from other build phases.
  - Emits JSIR intermediates (`prefixname.jsir`) rather than temporary `.cmj` files, saving the serialized IR before invoking `js_of_ocaml compile` and cleaning up the artifact afterwards.
  - Resolves archive inputs through `Load_path` so `.cmjxa`/`.cmja` creation mirrors native linking semantics.

### ocamltest Integration
- `ocamltest/ocaml_compilers.ml` defines compiler descriptors. Exposing the `flags` field through the tool interface ensures the JavaScript backend always inserts `-target js_of_ocaml` when ocamltest invokes `ocamlopt`. This is consumed in `ocamltest/ocaml_actions.ml` by prepending `Compiler.flags` to every command line.
- `ocamltest/ocaml_actions.ml` filters modules per backend (`filter_modules_for_backend`), removing `.c` stubs from JavaScript invocations and ensuring `.js` helpers are retained.
- JavaScript runs add two predicates up front: `libraries_are_javascript_compatible` guards against tests that depend on unsupported libs, and `javascript_supports_effects` skips whole suites (e.g., everything under `testsuite/tests/effects/`) because the current js runtime throws `Failure("Effect handlers are not supported")`. The predicate checks both the test source file and its directory components for an `effects` segment, avoiding per-test maintenance.
- With these predicates in place, running `env OCAMLTEST_BACKENDS=javascript make -s test` skips unsupported effect tests but keeps full coverage elsewhere, yielding a clean report without manual filtering.

### Practical Tips
- When adding new outputs or artifacts to the JavaScript backend, update `Optcomp_intf.File_extensions` first; the `Optcompile.Make` pipeline automatically uses those for dump file naming and artifact saving via `Unit_info`.
- Any new command-line flags that must always accompany a backend should be threaded through the tool descriptors (`Ocaml_compilers.compiler`) so ocamltest and the CLI stay in sync.
- If tests rely on auxiliary `.js` files, ensure `driver/compenv` categorises them correctly or they will be dropped before linking.
