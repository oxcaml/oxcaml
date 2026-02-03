/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Registration of global memory roots */

#ifndef CAML_GLOBROOTS_H
#define CAML_GLOBROOTS_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "roots.h"

void caml_scan_global_roots(scanning_action f, void* fdata);
void caml_scan_global_young_roots(scanning_action f, void* fdata);

#ifdef NATIVE_CODE
void caml_register_dyn_globals(void **globals, int nglobals);
#endif

#endif /* CAML_INTERNALS */

#ifdef NATIVE_CODE
/* Initialize a module and its dependencies by name.
   Requires the executable to be compiled with -manual-module-init.
   Raises an exception if the module is not found or if initialization fails. */
CAMLextern void caml_init_module(const char *name);
#endif

#endif /* CAML_GLOBROOTS_H */
