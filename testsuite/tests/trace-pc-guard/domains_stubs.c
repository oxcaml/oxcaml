/* Coverage callbacks for [-trace-pc-guard] that may be called from several
   domains at once. */

#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>

#include <caml/mlvalues.h>

int64_t __sancov_should_track = 0;

static uint32_t *guards_start = NULL;
static uint32_t *guards_stop = NULL;

void __sanitizer_cov_trace_pc_guard_init(uint32_t *start, uint32_t *stop)
{
  guards_start = start;
  guards_stop = stop;
}

static atomic_long calls = 0;
static atomic_long bad_guards = 0;

void __sanitizer_cov_trace_pc_guard(uint32_t *guard)
{
  if (guard < guards_start || guard >= guards_stop)
    atomic_fetch_add(&bad_guards, 1);
  else
    __atomic_store_n(guard, 1, __ATOMIC_RELAXED);
  atomic_fetch_add(&calls, 1);
}

value caml_tpcg_domains_set_gate(value enabled)
{
  __atomic_store_n(&__sancov_should_track, Bool_val(enabled),
                   __ATOMIC_SEQ_CST);
  return Val_unit;
}

value caml_tpcg_domains_num_calls(value unit)
{
  (void)unit;
  return Val_long(atomic_load(&calls));
}

value caml_tpcg_domains_num_bad_guards(value unit)
{
  (void)unit;
  return Val_long(atomic_load(&bad_guards));
}
