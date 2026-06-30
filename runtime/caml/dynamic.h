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

/* Define a new dynamic value, which is an immediate unique ID.
   If [inherit] is Val_true, fresh fibers snapshot the value of this dynamic. */
CAMLprim value caml_dynamic_make(value inherit);

/* Get the current value of a dynamic variable. Does not allocate. */
CAMLprim value caml_dynamic_get(value dyn);

/* Push a local binding for a dynamic variable.
   Must be paired with [caml_dynamic_pop] on the same fiber. */
CAMLprim value caml_dynamic_push(value dyn, value val);

/* Pop a local binding for a dynamic variable.
   Must be paired with [caml_dynamic_push] on the same fiber. */
CAMLprim value caml_dynamic_pop(value dyn);


typedef struct dynamic_binding_s {
  value dyn; /* Dynamic id, or Val_null if unbound */
  value val;
} dynamic_binding_s, *dynamic_binding_t;

/* If you change DYNAMIC_CACHE_BITS, you must also update the assembly-language
   stubs such as amd64.S.

   TODO: single source of truth for things like this. */
#define DYNAMIC_CACHE_BITS 3
#define DYNAMIC_CACHE_SIZE (1 << DYNAMIC_CACHE_BITS)

/* Per-thread cache of the most recently queried dynamic bindings.
   Layout must match Dynamic_ definitions in amd64.S.

   TODO: Stephen Dolan's wild plan to use vector instructions to do a fully-
   associative LRU cache. */
typedef struct dynamic_cache_s {
  dynamic_binding_s tbl[DYNAMIC_CACHE_SIZE];
} dynamic_cache_s, *dynamic_cache_t;

/* Allocate an empty dynamic cache. Returns NULL if allocation fails. */
extern dynamic_cache_t caml_dynamic_cache_new(void);

/* Deallocate a dynamic cache. */
extern void caml_dynamic_cache_delete(dynamic_cache_t);

/* Install a dynamic cache for this thread.
   Called by [st_stubs.c] upon switching threads. */
extern void caml_dynamic_cache_enter_thread(dynamic_cache_t);

/* Clear a dynamic cache.
   Called upon switching fibers or when the parent of the current fiber changes. */
extern void caml_dynamic_cache_flush(dynamic_cache_t);

/* Apply a GC scanning action to all bindings in a dynamic cache. */
extern void caml_dynamic_cache_scan_roots(dynamic_cache_t,
                                          scanning_action,
                                          scanning_action_flags,
                                          void *);


/* Each entry in a dynamic table is a stack that grows when full. */
typedef struct dynamic_stack_s *dynamic_stack_t;

/* Per-fiber hash table of local dynamic bindings.
   Maps dynamic ID to a stack of bindings installed on this fiber. */
typedef struct dynamic_table_s {
  size_t mask; /* capacity - 1 */
  size_t count;
  dynamic_stack_t bindings;
} dynamic_table_s, *dynamic_table_t;

/* Initialize a dynamic table to an empty state. */
extern void caml_dynamic_table_init(dynamic_table_t table);

/* Uninitialize a dynamic table, freeing any internal allocations. */
extern void caml_dynamic_table_free(dynamic_table_t table);

/* Duplicate a dynamic table. Returns false if allocation fails. */
extern bool caml_dynamic_table_dup(dynamic_table_t dst, dynamic_table_t src);

/* Bind the current value of every inheritable dynamic bound in the current
   stack or one of its parents. */
extern bool caml_dynamic_table_inherit(dynamic_table_t table);

/* Register all bindings as GC roots. */
extern void caml_dynamic_table_register_roots(dynamic_table_t table);

/* Unregister all bindings as GC roots. */
extern void caml_dynamic_table_unregister_roots(dynamic_table_t table);

/* Apply a GC scanning action to all bindings in a dynamic table. */
extern void caml_dynamic_table_scan_roots(dynamic_table_t,
                                          scanning_action,
                                          scanning_action_flags,
                                          void *);

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
