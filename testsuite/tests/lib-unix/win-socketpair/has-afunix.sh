#!/bin/sh

# Test if the OS runtime has afunix enabled.

if ! which sc > /dev/null 2>&1; then
  exit "${TEST_SKIP}"
fi

if sc query afunix > /dev/null; then
  exit "${TEST_PASS}";
fi
exit "${TEST_SKIP}"
