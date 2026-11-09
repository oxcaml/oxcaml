#include <caml/mlvalues.h>

/* Re-encode the reserved field from the scannable prefix the OCaml
   side claims, and compare with what the compiler emitted; then check
   the decoder recovers the prefix. */

value encode_identity_check(value v, value prefix_v)
{
  mlsize_t prefix = Long_val(prefix_v);
  reserved_t expected = Reserved_mixed_block_scannable_wosize_native(prefix);
  reserved_t got = Reserved_val(v);
  if (got != expected) return Val_false;
  if (Scannable_wosize_reserved_native(got, Allocated_wosize_val(v))
      != prefix)
    return Val_false;
  return Val_true;
}
