/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2023 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/simd.h"

#define Max_array_wosize                   (Max_wosize)

#define Max_unboxed_vec128_array_wosize    (Max_array_wosize / Words_per_vec128)
#define Max_unboxed_vec256_array_wosize    (Max_array_wosize / Words_per_vec256)

CAMLprim value caml_simd_bytecode_not_supported(void) {
  caml_fatal_error("SIMD is not supported in bytecode mode.");
}



CAMLprim value caml_unboxed_vec128_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
    memmove((uintnat *)a2 + Long_val(ofs2) * Words_per_vec128,
            (uintnat *)a1 + Long_val(ofs1) * Words_per_vec128,
            Long_val(n) * sizeof(uintnat) * Words_per_vec128);
    return Val_unit;
}

static value caml_make_unboxed_vec128_vect0(value len, int local)
{
  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_vec128_array_wosize)
    caml_invalid_argument("Array.make");

  /* Empty arrays have tag 0 */
  if (num_elements == 0) {
    return Atom(0);
  }

  mlsize_t num_fields = num_elements * Words_per_vec128;
  
  /* Mixed block with no scannable fields */
  reserved_t reserved = Reserved_mixed_block_scannable_wosize_native(0);

  if (local)
    return caml_alloc_local_reserved(num_fields, Unboxed_vec128_array_tag, reserved);
  else
    return caml_alloc_with_reserved(num_fields, Unboxed_vec128_array_tag, reserved);
}

CAMLprim value caml_make_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 0);
}

CAMLprim value caml_make_local_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 1);
}

CAMLprim value caml_make_unboxed_vec128_vect_bytecode(value len) {
  caml_failwith("128-bit SIMD is not supported on this platform.");
}


CAMLprim value caml_unboxed_vec256_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
    memmove((uintnat *)a2 + Long_val(ofs2) * Words_per_vec256,
            (uintnat *)a1 + Long_val(ofs1) * Words_per_vec256,
            Long_val(n) * sizeof(uintnat) * Words_per_vec256);
    return Val_unit;
}

static value caml_make_unboxed_vec256_vect0(value len, int local)
{
  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_vec256_array_wosize)
    caml_invalid_argument("Array.make");

  /* Empty arrays have tag 0 */
  if (num_elements == 0) {
    return Atom(0);
  }

  mlsize_t num_fields = num_elements * Words_per_vec256;
  
  /* Mixed block with no scannable fields */
  reserved_t reserved = Reserved_mixed_block_scannable_wosize_native(0);

  if (local)
    return caml_alloc_local_reserved(num_fields, Unboxed_vec256_array_tag, reserved);
  else
    return caml_alloc_with_reserved(num_fields, Unboxed_vec256_array_tag, reserved);
}

CAMLprim value caml_make_unboxed_vec256_vect(value len)
{
  return caml_make_unboxed_vec256_vect0(len, 0);
}

CAMLprim value caml_make_local_unboxed_vec256_vect(value len)
{
  return caml_make_unboxed_vec256_vect0(len, 1);
}

CAMLprim value caml_make_unboxed_vec256_vect_bytecode(value len) {
  caml_failwith("256-bit SIMD is not supported on this platform.");
}
