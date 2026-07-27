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

/* Runtime services on arrays.  The Array primitives are in
   stdlib/prims/array_prims.c. */

#include "caml/misc.h"
#include "caml/mlvalues.h"

/* returns number of elements (either fields or floats) */
/* [ 'a array -> int ] */
CAMLexport mlsize_t caml_array_length(value array)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else
#endif
    return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array)
{
  return (Tag_val(array) == Double_array_tag);
}
