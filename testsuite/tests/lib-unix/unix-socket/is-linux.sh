#!/bin/sh

# This script is related to the 'recvfrom_linux.ml' test.

uname="$(uname -s)"
if [ "$uname" = "Linux" ]; then
  exit ${TEST_PASS}
else
  echo "$uname" > "$ocamltest_response"
  exit ${TEST_SKIP}
fi
