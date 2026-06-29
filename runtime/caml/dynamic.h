/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                              Jane Street                               */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_DYNAMIC_H
#define CAML_DYNAMIC_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "roots.h"

typedef struct dynamic_cache_s *dynamic_cache_t;

/* Create a new dynamic cache */
extern dynamic_cache_t caml_dynamic_cache_new(void);

/* Delete a dynamic cache */
extern void caml_dynamic_cache_delete(dynamic_cache_t);

/* Install a new dynamic cache for this thread */
extern void caml_dynamic_cache_enter_thread(dynamic_cache_t);

/* Clear the cache. Used when switching fibers. */
extern void caml_dynamic_cache_flush(dynamic_cache_t);

/* Apply a GC scanning action to the roots in a dynamic cache. */
extern void caml_dynamic_cache_scan_roots(dynamic_cache_t,
                                          scanning_action,
                                          scanning_action_flags,
                                          void *);

typedef struct dynamic_stack_s *dynamic_stack_t;

typedef struct dynamic_table_s {
  size_t mask; /* capacity - 1 */
  size_t count;
  dynamic_stack_t bindings;
} dynamic_table_s, *dynamic_table_t;

/* Initialize a dynamic table */
extern void caml_dynamic_table_init(dynamic_table_t table);

/* Free a dynamic table */
extern void caml_dynamic_table_free(dynamic_table_t table);

/* Apply a GC scanning action to the roots in a dynamic table. */
extern void caml_dynamic_table_scan_roots(dynamic_table_t,
                                          scanning_action,
                                          scanning_action_flags,
                                          void *);

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
