#!/bin/sh
if grep 123456 "${1}"; then
  exit ${TEST_FAIL}
else
  exit ${TEST_PASS}
fi
