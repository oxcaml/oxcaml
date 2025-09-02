# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# OxCaml DWARF Test Suite

This directory contains tests for DWARF debugging information in the OxCaml compiler. The tests verify that OCaml programs compiled with debugging flags produce correct DWARF output that debuggers can use.

## Prerequisites

These tests require a custom LLDB build with OCaml support. The path to this custom LLDB must be provided via the `OXCAML_LLDB` environment variable when running tests.

## Architecture

**Test Components:**
- `test_*.ml` - OCaml source files with functions to debug
- `test_*.lldb` - LLDB debugger commands (breakpoints, continue, etc.)
- `test_*.output` - Expected filtered debugger output
- `filter_for_function_call_only.sh` - Normalizes LLDB output for stable comparisons
- `gen/gen_dune.ml` - Generates dune rules for all tests

**Build System:**
- `dune.inc` - Auto-generated from `gen/gen_dune.ml`, contains test rules
- Tests compile with `-g -gno-upstream-dwarf -shape-format debugging-shapes`
- Foreign C stubs compiled with SSE4.2/AVX2 support for SIMD tests

## Commands

```bash
# Run all DWARF tests from repository root
# Note: Individual tests cannot be run separately - all DWARF tests run together
OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf

# Regenerate dune.inc after modifying gen/gen_dune.ml (from repository root)
OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf
dune promote

# Update expected output after verifying changes (from repository root)
OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf
dune promote
```

## Important Restrictions

**NEVER manually build or run individual components:**
- Do NOT run `dune build oxcaml/tests/backend/oxcaml_dwarf/dune.inc.gen`
- Do NOT run `dune build oxcaml/tests/backend/oxcaml_dwarf/gen/gen_dune.exe`
- Do NOT execute test executables directly (e.g., `./test_*.exe`)
- Do NOT run LLDB commands manually on test executables

**All operations must go through `make runtest-dwarf`** - this ensures proper build dependencies, environment setup, and test orchestration. The make target handles the build and test pipeline correctly.

## Adding New Tests

1. Create `test_yourtest_dwarf.ml` with functions using:
   ```ocaml
   let[@inline never] [@local never] f_start () = ()
   let _ = f_start ()

   let[@inline never] [@local never] f_example (x : type) = ...
   let _ = f_example test_value
   ```

2. Create `test_yourtest_dwarf.lldb` with debugger commands:
   ```
   (lldb) b Test_yourtest_dwarf.f_start
   (lldb) run
   (lldb) b Test_yourtest_dwarf.f_example
   (lldb) c
   ```

3. Add test to `gen/gen_dune.ml`:
   ```ocaml
   print_dwarf_test "test_yourtest_dwarf";
   ```

4. Create an empty output file:
   ```bash
   touch test_yourtest_dwarf.output
   ```

5. Regenerate dune.inc and create initial output:
   ```bash
   # First iteration: generates and promotes the dune.inc file
   OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf
   dune promote
   # Second iteration: runs the actual tests and promotes the test output
   OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf
   dune promote
   ```

## Test Writing Patterns

**Preventing Optimization:**
- Use `[@inline never] [@local never]` to ensure functions aren't optimized away
- Keep test functions simple to isolate debugging behavior

**LLDB Script Format:**
- Each command on its own line prefixed with `(lldb)`
- Use module-qualified function names: `Test_module.function_name`
- Current tests follow a pattern: start with `f_start` dummy function breakpoint, then `run`, then other function breakpoints with `c` (continue)
- This pattern allows the `run` command to be issued once at the beginning, avoiding multiple runs
- Future tests may use different patterns as needed

**Output Filtering:**
- Addresses, PIDs, and paths are normalized to `<ADDRESS>`, `<PID>`, `<PATH>`
- LLDB prompts and thread information are removed
- Only function call frames are preserved

## Debugging Test Failures

When tests fail, the diff shows expected vs actual output:
```bash
# First, preprocess the LLDB script (removes "(lldb) " prefixes and empty lines)
sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_name.lldb > test_name_clean.lldb

# View the raw LLDB output before filtering
/path/to/custom/lldb -s test_name_clean.lldb ./test_name.exe

# Build and examine the corrected output
OXCAML_LLDB=/path/to/custom/lldb dune build test_name.output.corrected
cat _build/default/oxcaml/tests/backend/oxcaml_dwarf/test_name.output.corrected
```


## Finding Known Issues

Known DWARF debugging issues are documented as CR (Code Review) comments in the test files themselves. These appear as OCaml comments near the relevant test code and describe current limitations.

These CR comments provide context about current limitations and expected behavior when debugging OCaml programs with LLDB.
