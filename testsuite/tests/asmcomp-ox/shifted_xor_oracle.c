/* ACTUAL COMPILER: independent unsigned-word oracle for shifted XOR. */
#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define MASK63 UINT64_C(0x7fffffffffffffff)

CAMLprim value shifted_xor_oracle(value a, value b, value shift,
                                  value right, value offset)
{
  unsigned k = (unsigned)Long_val(shift);
  uint64_t x = (uint64_t)Long_val(a) & MASK63;
  uint64_t y = (uint64_t)Long_val(b) & MASK63;
  uint64_t shifted = Bool_val(right) ? y >> k : y << k;
  uint64_t result = x ^ ((shifted + (uint64_t)Long_val(offset)) & MASK63);
  return Val_long((intnat)result);
}

CAMLprim value shifted_xor_raw_oracle(value a, value b, value shift,
                                      value arithmetic)
{
  CAMLparam4(a, b, shift, arithmetic);
  unsigned k = (unsigned)Long_val(shift);
  uint64_t x = (uint64_t)Int64_val(a);
  uint64_t y = (uint64_t)Int64_val(b);
  uint64_t shifted = y >> k;
  if (Bool_val(arithmetic) && (y >> 63) && k != 0)
    shifted |= UINT64_MAX << (64 - k);
  uint64_t bits = x ^ shifted;
  int64_t result;
  memcpy(&result, &bits, sizeof result);
  CAMLreturn(caml_copy_int64(result));
}
