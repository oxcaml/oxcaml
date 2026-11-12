#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, Collège de France and Inria                   *
#*                                                                        *
#*   Copyright 2023 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Build the runtime/prims.c file, with proper C declarations of the primitives

export LC_ALL=C

case $# in
  0) echo "Usage: gen_primsc.sh <primitives file> <.c files>" 1>&2
     exit 2;;
  *) primitives="$1"; shift;;
esac

cat <<'EOF'
/* Generated file, do not edit */

#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/prims.h"

EOF

# Extract the beginning of primitive definitions:
# from 'CAMLprim' at beginning of line to the first closing parenthesis.
# The first pattern below matches single-line definitions such as
#    CAMLprim value foo(value x) {
# The second pattern matches multi-line definitions such as
#    CAMLprim value foo(value x,
#                       value y)
sed -n \
  -e '/^CAMLprim value .*)/p' \
  -e '/^CAMLprim value [^)]*$/,/)/p' \
  "$@" |
# Transform these definitions into "CAMLextern" declarations
# DEFINE_BYTE_UNOP, DEFINE_NAN_CMP and DEFINE_BYTE_BINOP are used in float32.c
sed \
  -e 's/ DEFINE_BYTE_UNOP([^)]*)/(value f)/' \
  -e 's/ DEFINE_NAN_CMP([^)]*)/(value f, value g)/' \
  -e 's/ DEFINE_BYTE_BINOP([^)]*)/(value f, value g)/' \
  -e '/^CAMLprim value [^ ][^ ]*$/s/prim/extern/' \
  -e '/^CAMLprim value [^ ][^ ]* *(/s/prim/extern/' \
  -e '/^CAMLprim .*/s/.*/#error "&"/' \
  -e 's/).*$/);/'

# Generate the table of primitives
echo
echo 'const c_primitive caml_builtin_cprim[] = {'
sed -e 's/.*/  (c_primitive) &,/' "$primitives"
echo '  0 };'

# Generate the table of primitive names
echo
echo 'const char * const caml_names_of_builtin_cprim[] = {'
sed -e 's/.*/  "&",/' "$primitives"
echo '  0 };'
