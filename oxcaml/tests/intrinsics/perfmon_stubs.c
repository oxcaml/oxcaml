#include "caml/mlvalues.h"
#include <assert.h>

#define BUILTIN(name)                                                          \
  void name() { assert(!"Didn't use [@@builtin] intrinsic."); }

BUILTIN(caml_rdtsc_unboxed)
BUILTIN(caml_arm64_read_cntvct_el0_unboxed)
BUILTIN(caml_pause_hint)
