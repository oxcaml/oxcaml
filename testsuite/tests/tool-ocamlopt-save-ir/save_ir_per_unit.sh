#!/bin/sh

first=save_ir_per_unit_first.cmir-cfg-regalloc
second=save_ir_per_unit.cmir-cfg-regalloc

if ! grep -q first_unit_function "$first"; then
    echo "first_unit_function not found in $first" > ${ocamltest_response}
    exit ${TEST_FAIL}
elif ! grep -q second_unit_function "$second"; then
    echo "second_unit_function not found in $second" > ${ocamltest_response}
    exit ${TEST_FAIL}
elif grep -q first_unit_function "$second"; then
    echo "first_unit_function found in $second" > ${ocamltest_response}
    exit ${TEST_FAIL}
else
    exit ${TEST_PASS}
fi
