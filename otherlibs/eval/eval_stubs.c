//******************************************************************************
//*                                  OxCaml                                    *
//* -------------------------------------------------------------------------- *
//*                               MIT License                                  *
//*                                                                            *
//* Copyright (c) 2025 Jane Street Group LLC                                   *
//* opensource-contacts@janestreet.com                                         *
//*                                                                            *
//* Permission is hereby granted, free of charge, to any person obtaining a    *
//* copy of this software and associated documentation files (the "Software"), *
//* to deal in the Software without restriction, including without limitation  *
//* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
//* and/or sell copies of the Software, and to permit persons to whom the      *
//* Software is furnished to do so, subject to the following conditions:       *
//*                                                                            *
//* The above copyright notice and this permission notice shall be included    *
//* in all copies or substantial portions of the Software.                     *
//*                                                                            *
//* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
//* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
//* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
//* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
//* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
//* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
//* DEALINGS IN THE SOFTWARE.                                                  *
//******************************************************************************

// CR metaprogramming jrickard: This file has not been code reviewed

#include <stdlib.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"

// From globroots.h; not declared there unless CAML_INTERNALS and NATIVE_CODE
// are defined.  This library only supports native code (it requires the
// JIT).
extern void caml_register_dyn_globals(void **globals, int nglobals);

extern char caml_bundled_cmis[] __attribute__((weak));
extern char caml_bundled_cmxs[] __attribute__((weak));
// CR mshinwell: it's a shame that "cmxs" clashes with the extension of
// a shared library file

value caml_bundled_cmis_this_exe()
{
  return (value) &caml_bundled_cmis;
}

value caml_bundled_cmxs_this_exe()
{
  return (value) &caml_bundled_cmxs;
}

value caml_bundle_available(value bundle)
{
  return (bundle == (value) 0) ? Val_false : Val_true;
}

// Allocate the module block of a compilation unit synthesised at runtime by
// [Eval] (value injection).  The block is allocated outside the OCaml heap,
// so its address is stable and can be bound to the unit's module block
// symbol for JIT-loaded code; it is registered as a dynamic global root
// (like the module blocks of natdynlink'd units), so its fields keep their
// contents alive and are updated if the GC moves them.  The block currently
// stays registered for the lifetime of the program (until code unloading is
// supported).
value caml_eval_alloc_injected_module_block(value fields)
{
  CAMLparam1(fields);
  mlsize_t wosize = Wosize_val(fields);
  mlsize_t i;
  // Header plus fields for the block itself, then the null-terminated
  // roots table of length 2 (see [scan_native_globals] in globroots.c).
  header_t *mem = malloc((1 + wosize) * sizeof(value) + 2 * sizeof(value));
  if (mem == NULL) caml_raise_out_of_memory();
  mem[0] = Caml_out_of_heap_header(wosize, 0);
  value block = Val_hp(mem);
  for (i = 0; i < wosize; i++) {
    Field(block, i) = Val_unit;
  }
  for (i = 0; i < wosize; i++) {
    // The minor GC does not scan dynamic global roots (see
    // [caml_scan_global_young_roots]), so stores of possibly-young values
    // into this out-of-heap block must go through [caml_initialize], which
    // records them in the remembered set.
    caml_initialize(&Field(block, i), Field(fields, i));
  }
  value *roots_table = (value *) (mem + 1 + wosize);
  roots_table[0] = block;
  roots_table[1] = 0;
  void *dyn_global = (void *) roots_table;
  caml_register_dyn_globals(&dyn_global, 1);
  CAMLreturn(block);
}

