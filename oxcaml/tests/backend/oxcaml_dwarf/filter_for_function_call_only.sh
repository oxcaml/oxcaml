#!/bin/bash

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
  -e 's|addr\s*=\s*0x[0-9a-f]*|addr=<ADDRESS>|g' \
  -e "s|'$PWD[^']*'|'<BUILD_DIR>'|g" \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml:[0-9]*:[0-9]*\( \[inlined\]\)$|\1|g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.mli:[0-9]*:[0-9]*\( \[inlined\]\)$|\1|g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.c:[0-9]*:[0-9]*\( \[inlined\]\)$|\1|g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.h:[0-9]*:[0-9]*\( \[inlined\]\)$|\1|g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml\.in:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.ml\.in:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.mli:[0-9]*:[0-9]*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.c:[0-9]*:[0-9]*.*$||g' \
  -e 's| at [a-zA-Z0-9_/.-]*\.h:[0-9]*:[0-9]*$||g' \
  -e 's|caml_start_program + [0-9]*|caml_start_program|g' \
  -e "s|inlined block 0x[0-9a-f]* doesn't have a range that contains file address 0x[0-9a-f]*|inlined block <HEX> doesn't have a range that contains file address <HEX>|g" \
  -e 's|caml_c_call + [0-9]*|caml_c_call + N|g' \
  -e 's|\([a-z_][a-zA-Z0-9_]*\)_[0-9]\+_[0-9]\+_code|\1_XXX_XXX_code|g' \
  -e 's|\([a-z_][a-zA-Z0-9_]*\)_[0-9]\+_unboxed|\1_XXX_unboxed|g' | \
# Second sed: Conceal unstable pointer values in C runtime function parameters
# Note: Previous LLDB plugin versions used OCaml formatting (#140737333164432L),
# current versions use native C formatting (140737333164432)
sed \
  -e '/caml_blit_bytes/s|s1=#\?[0-9]\{10,\}L\?|s1=<PTR>|g' \
  -e '/caml_blit_bytes/s|s2=#\?[0-9]\{10,\}L\?|s2=<PTR>|g' \
  -e '/caml_hash_exn/s|obj=#\?[0-9]\{5,\}L\?|obj=<PTR>|g' \
  -e '/caml_compare/s|v1=#\?[0-9]\{5,\}L\?|v1=<PTR>|g' \
  -e '/caml_compare/s|v2=#\?[0-9]\{5,\}L\?|v2=<PTR>|g' \
  -e '/caml_output_value_to_buffer/s|buf=#\?[0-9]\{10,\}L\?|buf=<PTR>|g' \
  -e '/caml_output_value_to_buffer/s|v=#\?[0-9]\{5,\}L\?|v=<PTR>|g' \
  -e '/caml_input_value_from_bytes/s|str=#\?[0-9]\{10,\}L\?|str=<PTR>|g' \
  -e '/caml_md5_string/s|str=#\?[0-9]\{5,\}L\?|str=<PTR>|g' \
  -e '/caml_sys_getenv/s|var=#\?[0-9]\{5,\}L\?|var=<PTR>|g' \
  -e '/caml_ba_create/s|vdim=#\?[0-9]\{5,\}L\?|vdim=<PTR>|g' | \
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
