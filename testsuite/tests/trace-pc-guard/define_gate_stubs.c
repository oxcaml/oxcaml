/* Coverage callbacks for [-gated-trace-pc-guard-define-gate]. */

#include <stdint.h>

#include <caml/mlvalues.h>

/* Overridden by the definition in the startup file. */
__attribute__((weak)) int64_t __sancov_should_track = 1;

void __sanitizer_cov_trace_pc_guard_init(uint32_t *start, uint32_t *stop)
{
  (void)start;
  (void)stop;
}

static long calls = 0;

void __sanitizer_cov_trace_pc_guard(uint32_t *guard)
{
  (void)guard;
  calls++;
}

value caml_tpcg_define_gate_set(value enabled)
{
  __sancov_should_track = Bool_val(enabled);
  return Val_unit;
}

value caml_tpcg_define_gate_num_calls(value unit)
{
  (void)unit;
  return Val_long(calls);
}
