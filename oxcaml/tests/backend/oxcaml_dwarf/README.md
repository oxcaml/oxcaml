# OxCaml DWARF Test Suite

This directory contains tests that verify DWARF debugging information produced by the OxCaml compiler.
Each test compiles an OCaml program with debugging flags, runs it under LLDB with a script of debugger commands, and compares the output against expected results. The output shows what LLDB displays when inspecting OCaml values, function calls, and program state.

**Running Tests** From the repository root:

```bash
OXCAML_LLDB=/path/to/custom/lldb make runtest-dwarf
```

Note: These tests require a custom LLDB build with OCaml support.

**Updating Test Output** After running tests, if there are differences:

```bash
make promote
```

For new tests, you may need to run `make runtest-dwarf` and `make promote` twice: once to generate dune rules, and once to capture the test output.

**Understanding Diffs** The test output represents what LLDB shows when debugging OCaml programs. This output should be stable and effectively only change when:

- An aspect of the OCaml language has changed (e.g., a new type system feature has been added)
- Debug information generation itself has been modified (e.g., to increase precision)

If you see unexpected diffs (especially non-renaming changes), there's a possibility that debug information is now missing or incorrect, so please take a look at how the output differs before committing your changes.
