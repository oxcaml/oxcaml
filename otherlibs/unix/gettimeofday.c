/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/sys.h>

/* Legacy primitives for external declarations using caml_unix_gettimeofday */

double caml_unix_gettimeofday_unboxed(value unit)
{
  return caml_sys_gettimeofday_unboxed(unit);
}

CAMLprim value caml_unix_gettimeofday(value unit)
{
  return caml_copy_double(caml_unix_gettimeofday_unboxed(unit));
}
