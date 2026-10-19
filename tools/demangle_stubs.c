/******************************************************************************
 *                                  OxCaml                                    *
 *                          Kevin Svetlitski, Jane Street                     *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************/

/* OCaml bindings for demangle.c, used by ocamlfilt --c-implementation. The
   demangler itself does not depend on the OCaml runtime, so that it can be
   reused as is by other tools. */

#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "demangle.h"

/* string -> string option */
CAMLprim value ocamlfilt_demangle_ocaml_symbol(value symbol)
{
  CAMLparam1(symbol);
  CAMLlocal1(demangled);
  const char *result =
    demangle_ocaml_symbol(caml_string_length(symbol), String_val(symbol));
  if (result == NULL) CAMLreturn(Val_none);
  demangled = caml_copy_string(result);
  free((void *)result);
  CAMLreturn(caml_alloc_some(demangled));
}
