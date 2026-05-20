#!/bin/sh
if grep 123456 "${1}"; then
  exit ${TEST_PASS}
else
  exit ${TEST_FAIL}
fi
