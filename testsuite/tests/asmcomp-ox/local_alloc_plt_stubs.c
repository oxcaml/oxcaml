#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/domain_state.h>

/* Model a PLT resolver before the runtime entry point saves registers.
   Use an offset inside the new arena so the unfixed compiler writes to the
   wrong slots rather than past the end of the arena. */
__asm__(
  ".text\n"
  ".globl __wrap_caml_call_local_realloc\n"
  ".type __wrap_caml_call_local_realloc, @function\n"
  "__wrap_caml_call_local_realloc:\n"
  "movq $-48, %r10\n"
  "movq $-48, %r11\n"
  "jmp __real_caml_call_local_realloc\n"
  ".size __wrap_caml_call_local_realloc, .-__wrap_caml_call_local_realloc\n"
);

static intnat offset_before;

CAMLprim value force_local_growth(value unit)
{
  offset_before = Caml_state->local_sp;
  /* Make the next allocation take the normal arena-growth path. */
  Caml_state->local_limit = offset_before;
  return Val_unit;
}

CAMLprim value reserved_pair_written(value unit)
{
  /* The allocation's region has closed, but no subsequent local allocation
     has reused these slots. Inspect them instead of the corrupted pointer. */
  value *top = (value *)((char *)Caml_state->local_top + offset_before);
  return Val_bool(top[-2] == Val_int(1) && top[-1] == Val_int(2));
}
