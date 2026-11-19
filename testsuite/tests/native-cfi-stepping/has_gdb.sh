#!/bin/sh

if ! which gdb >/dev/null 2>&1; then
    echo "gdb not found" > ${ocamltest_response}
    exit ${TEST_SKIP}
fi

exit ${TEST_PASS}
