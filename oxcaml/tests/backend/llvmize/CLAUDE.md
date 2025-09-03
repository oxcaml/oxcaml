# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# OxCaml LLVMIZE Test Suite

This directory contains tests for the LLVM backend (llvmize) in the OxCaml compiler. The tests verify that OCaml programs compiled with the LLVM backend produce correct output and generate expected LLVM IR.

## Prerequisites

These tests require a custom LLVM/Clang build with OxCaml support. The path to this custom Clang must be provided via the `OXCAML_CLANG` environment variable when running tests.

```bash
export OXCAML_CLANG=/path/to/custom/clang
```

## Architecture

**Test Components:**
- `test_*.ml` - OCaml source files to compile with LLVM backend
- `test_*_main.ml` - Main entry points that use the compiled modules
- `test_*_data.ml` / `test_*_defn.ml` - Supporting modules with data/definitions
- `test_*.output` - Expected program output
- `test_*_ir.output` - Expected filtered LLVM IR output
- `*.c` - C stubs for foreign function tests
- `filter.sh` - Normalizes LLVM IR output for stable comparisons
- `gen/gen_dune.ml` - Generates dune rules for all tests

**Build System:**
- `dune.inc` - Auto-generated from `gen/gen_dune.ml`, contains test rules
- Tests compile with `-llvm-backend -llvm-path ${OXCAML_CLANG}`
- IR output is filtered to remove volatile identifiers
- Foreign C stubs compiled with clang

## Commands

```bash
# Run all LLVMIZE tests from repository root
# Note: Individual tests cannot be run separately - all LLVMIZE tests run together
make runtest-llvmize

# Regenerate dune.inc after modifying gen/gen_dune.ml
# This happens automatically when running make runtest-llvmize
make runtest-llvmize
make promote

# Update expected output after verifying changes
make runtest-llvmize
make promote
```


## Important Restrictions

**NEVER manually build or run individual components:**
- Do NOT run `dune build oxcaml/tests/backend/llvmize/dune.inc.gen`
- Do NOT run `dune build oxcaml/tests/backend/llvmize/gen/gen_dune.exe`
- Do NOT execute test executables directly (e.g., `./test_*.exe`)
- Do NOT run LLVM or CLANG commands manually on test executables

**All operations must go through `make runtest-llvm`** - this ensures proper build dependencies, environment setup, and test orchestration. The make target handles the build and test pipeline correctly.


## Test Patterns

### Simple IR Test (no execution)
Tests that only verify LLVM IR generation:
```ocaml
(* id_fn.ml *)
let id x = x
```

### IR Test with Execution
Tests that verify both IR generation and program execution:
```ocaml
(* const_val.ml *)
let x = 37

(* const_val_main.ml *)
let () = print_int Const_val.x; print_newline ()
```

### Test with Data Dependencies
Tests with separate data modules:
```ocaml
(* gcd.ml - compiled with LLVM *)
let rec gcd a b = ...

(* gcd_data.ml - compiled normally *)
let test_data = [(12, 8); (35, 14)]

(* gcd_main.ml - compiled normally *)
let () = List.iter test_gcd Gcd_data.test_data
```

### Test with C Stubs
Tests with foreign C functions:
```ocaml
(* extcalls.ml - compiled with LLVM *)
external my_function : int -> int = "stub_function"

(* extcalls_defn.c - compiled with clang *)
value stub_function(value x) { ... }
```

## Adding New Tests

1. Create test files following naming conventions:
   - `test_name.ml` - Main module for LLVM compilation
   - `test_name_main.ml` - Entry point (if test should execute)
   - `test_name.output` - Expected stdout (if test executes)

2. Add test to `gen/gen_dune.ml`:
   ```ocaml
   print_test_ir_and_run "test_name";  (* For IR + execution *)
   print_test_ir_only "test_name";     (* For IR only *)
   ```

3. Create empty output files:
   ```bash
   touch test_name.output
   touch test_name_ir.output  # If applicable
   ```

4. Regenerate dune.inc and run tests:
   ```bash
   export OXCAML_CLANG=/path/to/custom/clang
   # First run: regenerates and promotes the dune.inc file
   make runtest-llvmize
   make promote
   # Second run: runs the actual tests and promotes the test output
   make runtest-llvmize
   make promote
   ```

## Test Writing Patterns

**Preventing Optimization:**
- Use `[@inline never] [@local never]` to ensure functions aren't optimized away
- Keep test functions simple

## Important Notes

- Tests require AMD64 architecture
- OXCAML_CLANG environment variable must be set
- IR output is filtered to remove non-deterministic elements
- All tests run with `-O3 -g` optimization flags
- Poll insertion is currently disabled (`-disable-poll-insertion`)

## Test Categories

**Basic Operations:**
- `const_val` - Integer constants
- `float_ops` - Floating point operations
- `int_ops` - Integer operations with C stubs

**Control Flow:**
- `gcd` - Recursive functions
- `switch` - Pattern matching
- `csel` - Conditional selection
- `tailcall` - Tail call optimization

**Data Structures:**
- `array_rev` - Array operations
- `alloc` - Allocation and boxing
- `data_decl` - Data declarations

**Function Calls:**
- `indirect_call` - Indirect/higher-order functions
- `many_args` - Functions with many arguments
- `multi_ret` - Multiple return values
- `extcalls` - External C calls

**Exception Handling:**
- `exn` - Exception raising and handling

## Debugging Test Failures

When tests fail, the diff shows expected vs actual output:

```bash
# View the generated LLVM IR
cat _build/default/oxcaml/tests/backend/llvmize/test_name.ll

# View the filtered IR output
cat _build/default/oxcaml/tests/backend/llvmize/test_name_ir.output.corrected

# View execution output
cat _build/default/oxcaml/tests/backend/llvmize/test_name.output.corrected
```

Common issues:
- Missing OXCAML_CLANG environment variable
- Non-deterministic IR elements not properly filtered
- Changes in LLVM IR format between versions
