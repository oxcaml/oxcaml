#!/bin/bash

# Filter script for DWARF test output to show only function call frames
# Removes LLDB commands, process status messages, thread info, and source locations
# All source file locations (.ml, .mli, .c, .h) are filtered for test stability

sed \
  -e 's|/[^[:space:]]*/\([^/]*\.exe\)|<PATH>/\1|g' \
  -e 's|Process [0-9]*|Process <PID>|g' \
  -e 's|address = 0x[0-9a-f]*|address = <ADDRESS>|g' \
  -e 's|frame #[0-9]*: 0x[0-9a-f]*|frame #N: <ADDRESS>|g' \
  -e 's|argv=0x[0-9a-f]*|argv=<ADDRESS>|g' \
  -e 's|@0x[0-9a-f]*|@<ADDRESS>|g' \
  -e "s|'$PWD[^']*'|'<BUILD_DIR>'|g" \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml:[0-9]*:[0-9]*.*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.mli:[0-9]*:[0-9]*.*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.c:[0-9]*:[0-9]*.*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.h:[0-9]*:[0-9]*.*$||g' \
  -e 's|caml_start_program.*|<Startup code ...>|g' \
  -e 's|\([a-z_][a-zA-Z0-9_]*\)_[0-9]\+_unboxed|\1_XXX_unboxed|g' | \
grep -v \
  -e '^(lldb) ' \
  -e '^Process <PID> resuming$' \
  -e '^Process <PID> stopped$' \
  -e '^Process <PID> launched:' \
  -e '^\* thread #[0-9]*, name = ' \
  -e '^Breakpoint [0-9]*:' \
  -e '^Current executable set to' \
  -e '^Executing commands in' \
  -e '^$' \
  -e 'caml_startup_common' \
  -e 'caml_startup_exn' \
  -e 'caml_startup' \
  -e 'caml_main' \
  -e 'libc.so.6' \
  -e '\.exe`_start' \
  -e '___lldb_unnamed_symbol' \
  -e '^    frame #N: <ADDRESS> test_.*\.exe`main' | \
tr -d '\r'