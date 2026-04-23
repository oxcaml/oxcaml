#!/usr/bin/env bash

# Filter script for DWARF test output to show only function call frames
# Removes LLDB commands, process status messages, thread info, and source locations
# All source file locations (.ml, .mli, .c, .h) are filtered for test stability

# First sed: General address/path normalization
sed \
  -e 's|/[^[:space:]]*/\([^/]*\.exe\)|<PATH>/\1|g' \
  -e 's|Process [0-9]*|Process <PID>|g' \
  -e 's|address = 0x[0-9a-f]*|address = <ADDRESS>|g' \
  -e 's|frame #[0-9]*: 0x[0-9a-f]*|frame #N: <ADDRESS>|g' \
  -e 's|argv=0x[0-9a-f]*|argv=<ADDRESS>|g' \
  -e 's|@0x[0-9a-f]*|@<ADDRESS>|g' \
  -e 's|data=0x[0-9a-f]*|data=<ADDRESS>|g' \
  -e "s|'$PWD[^']*'|'<BUILD_DIR>'|g" \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml\.in:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml\.in:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.mli:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.c:[0-9]*:[0-9]*.*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.h:[0-9]*:[0-9]*$||g' \
  -e 's|caml_start_program + [0-9]*|caml_start_program|g' \
  -e 's|caml_c_call + [0-9]*|caml_c_call + N|g' \
  -e 's|\([a-z_][a-zA-Z0-9_]*\)_[0-9]\+_[0-9]\+_code|\1_XXX_XXX_code|g' \
  -e 's|\([a-z_][a-zA-Z0-9_]*\)_[0-9]\+_unboxed|\1_XXX_unboxed|g' | \
# Second sed: Conceal unstable pointer values in C runtime function parameters
sed \
  -e 's|#[0-9]*[02468]L : value|<PTR> : value|g' | \
# grep: Remove LLDB noise
grep -v \
  -e '^(lldb) ' \
  -e '^Process <PID> resuming$' \
  -e '^Process <PID> stopped$' \
  -e '^Process <PID> launched:' \
  -e '^\* thread #[0-9]*, name = ' \
  -e '^Breakpoint [0-9]*:' \
  -e '^Current executable set to' \
  -e '^Executing commands in' \
  -e '^$' | \
tr -d '\r'
