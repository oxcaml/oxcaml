#!/bin/sh

# Passes where the runtime can grow a stack and gives it a guard page
# otherwise, which is what overflow.ml tests. A runtime configured with
# --enable-mmap-map-stack does neither, in any codegen variant: it
# allocates each stack once, at its initial size (see alloc_for_stack and
# caml_try_realloc_stack in runtime/fiber.c), which the first recursion of
# the test exceeds.

if grep -q '^#define USE_MMAP_MAP_STACK' "$ocamlsrcdir/runtime/caml/m.h"; then
  echo "stacks cannot grow: configured with --enable-mmap-map-stack" \
    > "$ocamltest_response"
  exit "${TEST_SKIP:-125}"
fi
exit "${TEST_PASS:-0}"
