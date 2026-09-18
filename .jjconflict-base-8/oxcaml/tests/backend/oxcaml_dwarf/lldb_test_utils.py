"""Utilities shared by the Python DWARF tests in this directory.

These tests run inside LLDB's embedded script interpreter:

  lldb --batch -o "command script import test_foo.py"

and use the SB API to make structured assertions about debugger state,
instead of pattern-matching LLDB's textual output (which is not a stable
interface; see lldb/docs/resources/test.rst in the LLDB sources).
"""


def source_pos(filepath, fragment):
    """Position of the first occurrence of [fragment] in [filepath], as a
    (line, column) pair matching the compiler's line table (1-based line,
    0-based column). Lets tests name source positions by the text they refer
    to, instead of hardcoding numbers that silently rot as the file is
    edited."""
    with open(filepath) as f:
        for lineno, line in enumerate(f, start=1):
            column = line.find(fragment)
            if column >= 0:
                return lineno, column
    raise AssertionError(f"{fragment!r} does not occur in {filepath}")


def run(main):
    """Test entry point: call as the last line of the test file.

    The test file is loaded with `command script import`, so an exception
    escaping [main] fails the import, which in batch mode makes LLDB exit
    nonzero after printing the traceback; returning normally lets LLDB
    exit 0."""
    main()
    print("PASS")
