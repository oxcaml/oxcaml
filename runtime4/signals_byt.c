/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Signal handling, code specific to the bytecode interpreter */

#include <signal.h>
#include <errno.h>
#include "caml/config.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/signals_machdep.h"

#ifndef NSIG
#define NSIG 64
#endif

CAMLexport void * caml_setup_stack_overflow_detection(void) { return NULL; }
CAMLexport int caml_stop_stack_overflow_detection(void * p) { return 0; }
CAMLexport void caml_init_signals(void) { }
CAMLexport void caml_terminate_signals(void) { }
