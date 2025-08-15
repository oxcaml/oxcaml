#include <assert.h>
#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_int_popcnt_untagged_to_untagged)
BUILTIN(caml_int_ctz_untagged_to_untagged)
BUILTIN(caml_int_clz_untagged_to_untagged)
