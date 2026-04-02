/* Coverage callbacks for [-trace-pc-guard], playing the part of the runtime
   that would normally be provided (e.g. by Antithesis). */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/mlvalues.h>

int64_t __sancov_should_track = 0;

static uint32_t *guards_start = NULL;
static uint32_t *guards_stop = NULL;
static int init_calls = 0;

void __sanitizer_cov_trace_pc_guard_init(uint32_t *start, uint32_t *stop)
{
  init_calls++;
  guards_start = start;
  guards_stop = stop;
  for (uint32_t *g = start; g < stop; g++) *g = 0;
}

static int misaligned_calls = 0;

void __sanitizer_cov_trace_pc_guard(uint32_t *guard)
{
  if (guard < guards_start || guard >= guards_stop) {
    fprintf(stderr, "guard %p outside of [%p, %p)\n", (void *)guard,
            (void *)guards_start, (void *)guards_stop);
    abort();
  }
  *guard = 1;
#if defined(__x86_64__)
  /* The frame pointer of this function is 16-byte aligned iff the caller
     aligned the stack as the C calling convention requires. */
  if ((uintptr_t)__builtin_frame_address(0) % 16 != 0) misaligned_calls++;
  /* Overwrite every register that the C calling convention lets a callee
     clobber, so that the test fails if the probe does not save one that is
     live. */
  __asm__ volatile(
    "mov $-1, %%rax\n\tmov $-1, %%rcx\n\tmov $-1, %%rdx\n\t"
    "mov $-1, %%rsi\n\tmov $-1, %%rdi\n\tmov $-1, %%r8\n\t"
    "mov $-1, %%r9\n\tmov $-1, %%r10\n\tmov $-1, %%r11\n\t"
    "pcmpeqd %%xmm0, %%xmm0\n\tpcmpeqd %%xmm1, %%xmm1\n\t"
    "pcmpeqd %%xmm2, %%xmm2\n\tpcmpeqd %%xmm3, %%xmm3\n\t"
    "pcmpeqd %%xmm4, %%xmm4\n\tpcmpeqd %%xmm5, %%xmm5\n\t"
    "pcmpeqd %%xmm6, %%xmm6\n\tpcmpeqd %%xmm7, %%xmm7\n\t"
    "pcmpeqd %%xmm8, %%xmm8\n\tpcmpeqd %%xmm9, %%xmm9\n\t"
    "pcmpeqd %%xmm10, %%xmm10\n\tpcmpeqd %%xmm11, %%xmm11\n\t"
    "pcmpeqd %%xmm12, %%xmm12\n\tpcmpeqd %%xmm13, %%xmm13\n\t"
    "pcmpeqd %%xmm14, %%xmm14\n\tpcmpeqd %%xmm15, %%xmm15"
    :
    :
    : "rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "xmm0",
      "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9",
      "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", "cc");
#endif
}

extern uint32_t __start_ocaml_trace_pc_guards[] __attribute__((weak));
extern uint32_t __stop_ocaml_trace_pc_guards[] __attribute__((weak));

__attribute__((constructor)) static void register_guards(void)
{
  if (__start_ocaml_trace_pc_guards != __stop_ocaml_trace_pc_guards)
    __sanitizer_cov_trace_pc_guard_init(__start_ocaml_trace_pc_guards,
                                        __stop_ocaml_trace_pc_guards);
}

value caml_tpcg_set_gate(value enabled)
{
  __sancov_should_track = Bool_val(enabled);
  return Val_unit;
}

value caml_tpcg_reset(value unit)
{
  (void)unit;
  for (uint32_t *g = guards_start; g < guards_stop; g++) *g = 0;
  return Val_unit;
}

value caml_tpcg_num_guards(value unit)
{
  (void)unit;
  return Val_long(guards_stop - guards_start);
}

value caml_tpcg_num_hit(value unit)
{
  (void)unit;
  long n = 0;
  for (uint32_t *g = guards_start; g < guards_stop; g++) n += *g != 0;
  return Val_long(n);
}

value caml_tpcg_init_calls(value unit)
{
  (void)unit;
  return Val_int(init_calls);
}

value caml_tpcg_misaligned_calls(value unit)
{
  (void)unit;
  return Val_int(misaligned_calls);
}

/* The closures of [@@builtin] externals reference their C names, although
   native code never calls them. */
#define BUILTIN(name) \
  void name(void) { abort(); }

BUILTIN(caml_vec128_unreachable);
BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_sse2_int64x2_add);
BUILTIN(caml_float64x2_low_of_float);
BUILTIN(caml_float64x2_low_to_float);
BUILTIN(caml_sse2_float64x2_add);
