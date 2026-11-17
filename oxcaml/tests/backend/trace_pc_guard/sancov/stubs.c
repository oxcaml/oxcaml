/* This file is compiled by clang with [-fsanitize-coverage=trace-pc-guard],
   so the executable mixes guards and constructors from clang and OxCaml. */

#include <stdint.h>

#include <caml/mlvalues.h>

/* Coverage runtimes other than compiler-rt's set the gate themselves. */
int64_t __sancov_should_track = 1;

value hit_cstub(value unit) { return unit; }

value missed_cstub(value unit) { return unit; }
