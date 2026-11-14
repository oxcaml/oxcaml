#include <caml/mlvalues.h>

/* Re-encode the reserved field from the scannable prefix the OCaml
   side claims, and compare with what the compiler emitted; then check
   that the runtime recovers the prefix. */

value caml_succ_scannable_prefix_len(value v);

value encode_identity_check(value v, value prefix_v)
{
  mlsize_t prefix = Long_val(prefix_v);
  reserved_t expected =
    Reserved_mixed_block_scannable_wosize_native(prefix,
                                                 Allocated_wosize_val(v));
  reserved_t got = Reserved_val(v);
  if (got != expected) return Val_false;
  if (caml_succ_scannable_prefix_len(v) != Val_long(prefix + 1))
    return Val_false;
  return Val_true;
}
