#!/bin/sh

# Skip unless a C program, linked like the OCaml ones, can walk its own stack
# with _Unwind_Backtrace and find the function of a frame.

dir=$(mktemp -d) || exit ${TEST_SKIP}

cat > "$dir/probe.c" <<PROBE
#include <unwind.h>

static int found;

static _Unwind_Reason_Code visit(struct _Unwind_Context *ctx, void *data)
{
  if (_Unwind_FindEnclosingFunction((void *)_Unwind_GetIP(ctx)) == data)
    found = 1;
  return _URC_NO_REASON;
}

__attribute__((noinline)) static int walk(void *caller)
{
  _Unwind_Backtrace(visit, caller);
  return found;
}

int main(void)
{
  return walk((void *)main) ? 0 : 1;
}
PROBE

if ${mkexe} -o "$dir/probe" "$dir/probe.c" > /dev/null 2>&1 \
   && ASAN_OPTIONS=detect_leaks=0 "$dir/probe" > /dev/null 2>&1; then
    result=${TEST_PASS}
else
    echo "_Unwind_Backtrace does not work" > ${ocamltest_response}
    result=${TEST_SKIP}
fi
rm -r "$dir"
exit $result
