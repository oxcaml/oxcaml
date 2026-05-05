/* C stubs for the Linux `perf` jitdump format.

   On non-Linux platforms the entire feature is disabled at runtime via
   [caml_perf_jitdump_is_linux] returning [false]; the other stubs are
   compiled in but should never be called. They are still safe to call
   (returning failure / no-op) so the OCaml side does not need conditional
   externals. */

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/fail.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __linux__
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#endif

CAMLprim value caml_perf_jitdump_is_linux(value unit) {
  (void)unit;
#ifdef __linux__
  return Val_true;
#else
  return Val_false;
#endif
}

/* Returns nanoseconds since CLOCK_MONOTONIC origin, the timestamp source
   recommended by the jitdump spec. Must match `perf record -k mono`. */
CAMLprim value caml_perf_jitdump_clock_monotonic(value unit) {
  (void)unit;
#ifdef __linux__
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    caml_failwith("perf_jitdump: clock_gettime(CLOCK_MONOTONIC) failed");
  }
  uint64_t ns = (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
  return caml_copy_int64((int64_t)ns);
#else
  return caml_copy_int64(0);
#endif
}

/* mmap [size] bytes of [fd] at offset 0 with PROT_READ | PROT_EXEC. The
   resulting mapping is the "marker" `perf record` captures as a
   PERF_RECORD_MMAP, letting `perf inject --jit` later locate the dump file
   by path. The mapping is intentionally never unmapped — the kernel reaps it
   at process exit, and saved perf.data must remain replayable.

   Best-effort: if mmap fails, we silently continue. The dump still gets
   written to disk; only the marker is missing, so `perf inject` will not
   find it automatically. */
CAMLprim value caml_perf_jitdump_mmap_marker(value v_fd, value v_size) {
#ifdef __linux__
  int fd = Int_val(v_fd);
  size_t size = (size_t)Long_val(v_size);
  void *p = mmap(NULL, size, PROT_READ | PROT_EXEC, MAP_PRIVATE, fd, 0);
  (void)p; /* leak intentional */
#else
  (void)v_fd;
  (void)v_size;
#endif
  return Val_unit;
}
