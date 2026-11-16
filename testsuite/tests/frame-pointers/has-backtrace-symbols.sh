#!/bin/sh

# fp_backtrace.c symbolizes return addresses with backtrace_symbols from
# <execinfo.h> and refers to REG_STARTEND from <regex.h>.  Not every C
# library provides them (musl has neither), so the tests that compile
# fp_backtrace.c are skipped when the C compiler rejects this probe.

if ${cc} -fsyntax-only -x c - > /dev/null 2>&1 <<EOF_PROBE
#include <execinfo.h>
#include <regex.h>
int probe(void *const *addr)
{
  return REG_STARTEND + (backtrace_symbols(addr, 1) != 0);
}
EOF_PROBE
then
  exit ${TEST_PASS}
else
  echo "the C library lacks backtrace_symbols or REG_STARTEND" \
    > "${ocamltest_response}"
  exit ${TEST_SKIP}
fi
