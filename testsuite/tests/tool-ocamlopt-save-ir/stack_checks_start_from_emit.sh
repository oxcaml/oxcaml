#!/bin/sh

# The instructions of an assembly file (the lines that start with white space
# and a letter), without the numbers of local labels.
instructions () {
    grep '^[[:space:]][[:space:]]*[a-z]' "$1" | sed 's/\.*L[0-9][0-9]*/L/g'
}

instructions from_source.${asmext} > from_source.txt
instructions stack_checks_start_from_emit.${asmext} > from_ir.txt

if [ -s from_source.txt ] && cmp -s from_source.txt from_ir.txt; then
    exit ${TEST_PASS}
else
    echo "the instructions from the source and from the IR differ" \
         > ${ocamltest_response}
    exit ${TEST_FAIL}
fi
