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

#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/prims.h"

/* One reference into each of the stdlib C primitive files under
   stdlib/prims/, so that linking this object pulls all of them out of
   libasmprims.  ocamlopt links this object into every native
   executable, mirroring the unconditional presence of the stdlib
   primitives in the bytecode runtime's builtin primitive table
   (prims.c): dynamically-loaded OCaml code may use any of these
   primitives even when the main program does not. */
CAMLextern value caml_array_get_addr(value, value);
CAMLextern value caml_ba_create(value, value, value);
CAMLextern value caml_blake2_create(value, value);
CAMLextern value caml_compare(value, value);
CAMLextern value caml_float32_of_float(value);
CAMLextern value caml_format_float(value, value);
CAMLextern value caml_gc_quick_stat(value);
CAMLextern value caml_hash_exn(value, value, value, value);
CAMLextern value caml_bswap16(value);
CAMLextern value caml_ml_flush(value);
CAMLextern value caml_lex_engine(value, value, value);
CAMLextern value caml_md5_string(value, value, value);
CAMLextern value caml_obj_raw_field(value, value);
CAMLextern value caml_parse_engine(value, value, value, value);
CAMLextern value caml_lxm_next(value);
CAMLextern value caml_ml_string_length(value);
CAMLextern value caml_sys_getenv(value);

const c_primitive caml_stdlib_prims_force_link[] = {
  (c_primitive) &caml_array_get_addr,   /* array_prims.c */
  (c_primitive) &caml_ba_create,        /* bigarray_prims.c */
  (c_primitive) &caml_blake2_create,    /* blake2.c */
  (c_primitive) &caml_compare,          /* compare.c */
  (c_primitive) &caml_float32_of_float, /* float32_prims.c */
  (c_primitive) &caml_format_float,     /* floats_prims.c */
  (c_primitive) &caml_gc_quick_stat,    /* gc_ctrl_prims.c */
  (c_primitive) &caml_hash_exn,         /* hash_prims.c */
  (c_primitive) &caml_bswap16,          /* ints_prims.c */
  (c_primitive) &caml_ml_flush,         /* io_prims.c */
  (c_primitive) &caml_lex_engine,       /* lexing.c */
  (c_primitive) &caml_md5_string,       /* md5_prims.c */
  (c_primitive) &caml_obj_raw_field,    /* obj_prims.c */
  (c_primitive) &caml_parse_engine,     /* parsing.c */
  (c_primitive) &caml_lxm_next,         /* prng.c */
  (c_primitive) &caml_ml_string_length, /* str_prims.c */
  (c_primitive) &caml_sys_getenv,       /* sys_prims.c */
};
