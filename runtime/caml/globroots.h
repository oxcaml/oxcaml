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

/* Remove [global] from the dyn-global skiplist. Used by the unloadable-CU
 * activation path ([caml_activate_unloadable_unit]) to undo a
 * [caml_register_dyn_globals] once the unit's static blocks have been
 * donated to the major heap (from which point ordinary marking scans their
 * fields, and a permanent root registration would pin the unit forever).
 * Returns 1 if [global] was present, 0 otherwise. */
int caml_unregister_dyn_global(void *global);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_GLOBROOTS_H */
