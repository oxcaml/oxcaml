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
#elif defined(__aarch64__)
  if ((uintptr_t)__builtin_frame_address(0) % 16 != 0) misaligned_calls++;
  /* As above. A callee must preserve the low halves of v8-v15 only: the
     compiler saves and restores those, so that the high halves end up
     clobbered. */
  __asm__ volatile(
    "mov x0, #-1\n\tmov x1, #-1\n\tmov x2, #-1\n\tmov x3, #-1\n\t"
    "mov x4, #-1\n\tmov x5, #-1\n\tmov x6, #-1\n\tmov x7, #-1\n\t"
    "mov x8, #-1\n\tmov x9, #-1\n\tmov x10, #-1\n\tmov x11, #-1\n\t"
    "mov x12, #-1\n\tmov x13, #-1\n\tmov x14, #-1\n\tmov x15, #-1\n\t"
    "mov x16, #-1\n\tmov x17, #-1\n\t"
    "movi v0.16b, #255\n\tmovi v1.16b, #255\n\tmovi v2.16b, #255\n\t"
    "movi v3.16b, #255\n\tmovi v4.16b, #255\n\tmovi v5.16b, #255\n\t"
    "movi v6.16b, #255\n\tmovi v7.16b, #255\n\tmovi v8.16b, #255\n\t"
    "movi v9.16b, #255\n\tmovi v10.16b, #255\n\tmovi v11.16b, #255\n\t"
    "movi v12.16b, #255\n\tmovi v13.16b, #255\n\tmovi v14.16b, #255\n\t"
    "movi v15.16b, #255\n\tmovi v16.16b, #255\n\tmovi v17.16b, #255\n\t"
    "movi v18.16b, #255\n\tmovi v19.16b, #255\n\tmovi v20.16b, #255\n\t"
    "movi v21.16b, #255\n\tmovi v22.16b, #255\n\tmovi v23.16b, #255\n\t"
    "movi v24.16b, #255\n\tmovi v25.16b, #255\n\tmovi v26.16b, #255\n\t"
    "movi v27.16b, #255\n\tmovi v28.16b, #255\n\tmovi v29.16b, #255\n\t"
    "movi v30.16b, #255\n\tmovi v31.16b, #255"
    :
    :
    : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10",
      "x11", "x12", "x13", "x14", "x15", "x16", "x17", "v0", "v1", "v2", "v3",
      "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14",
      "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24",
      "v25", "v26", "v27", "v28", "v29", "v30", "v31", "cc");
#endif
}

extern uint32_t __start_oxcaml_trace_pc_guards[] __attribute__((weak));
extern uint32_t __stop_oxcaml_trace_pc_guards[] __attribute__((weak));

__attribute__((constructor)) static void register_guards(void)
{
  if (&__start_oxcaml_trace_pc_guards[0] != &__stop_oxcaml_trace_pc_guards[0])
    __sanitizer_cov_trace_pc_guard_init(__start_oxcaml_trace_pc_guards,
                                        __stop_oxcaml_trace_pc_guards);
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
BUILTIN(caml_simd_int64x2_add);
BUILTIN(caml_float64x2_low_of_float);
BUILTIN(caml_float64x2_low_to_float);
