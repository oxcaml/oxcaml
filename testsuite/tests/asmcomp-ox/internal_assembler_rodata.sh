#!/bin/sh

if ! which objdump >/dev/null 2>&1; then
    echo "objdump not found" > ${ocamltest_response}
    exit ${TEST_SKIP}
fi

# objdump prints each section's flags on the line after its header.
objdump -h "$1" | grep -A1 -E ' \.rodata +' | grep -q ALLOC
