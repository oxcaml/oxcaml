/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MINOR_GC_STRUCTS_H
#define CAML_MINOR_GC_STRUCTS_H

#include "mlvalues.h"

struct caml_ref_table {
# define TABLE_TYPE value *
# include "table_struct.tbl"
};

struct caml_ephe_ref_elt {
  value ephe;      /* an ephemeron in major heap */
  mlsize_t offset; /* the offset that points in the minor heap  */
  value stash;     /* copy of the field if it requires minor-GC cleaning */
};

struct caml_ephe_ref_table {
# define TABLE_TYPE struct caml_ephe_ref_elt
# include "table_struct.tbl"
};

struct caml_custom_elt {
  value block;     /* The finalized block in the minor heap. */
};

struct caml_custom_table {
# define TABLE_TYPE struct caml_custom_elt
# include "table_struct.tbl"
};

struct caml_dependent_elt {
  value block;     /* The finalized block in the minor heap. */
  mlsize_t mem;    /* The size in bytes of the dependent memory. */
};

struct caml_dependent_table {
# define TABLE_TYPE struct caml_dependent_elt
# include "table_struct.tbl"
};

struct caml_minor_tables {
# include "minor_tables.tbl"
};

#endif /* CAML_MINOR_GC_STRUCTS_H */
