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

/* Define a new dynamic value, which is an immediate unique ID. */
CAMLprim value caml_dynamic_make(value unit);

/* Get the current value of a dynamic variable. Does not allocate. */
CAMLprim value caml_dynamic_get(value dyn);

/* Push a local binding for a dynamic variable.
   Must be paired with [caml_dynamic_pop] on the same fiber. */
CAMLprim value caml_dynamic_push(value dyn, value val);

/* Pop the fiber's most recent local binding, which must be for [dyn].
   Must be paired with [caml_dynamic_push] on the same fiber. */
CAMLprim value caml_dynamic_pop(value dyn);

<<<<<<< HEAD
/* Return a handle to the dynamic binding state currently visible to this fiber.

   These functions are intended to be used by structured concurrency
   primitives in the following order:

   let scope = caml_dynamic_freeze_scope () in
   let l, r =
      spawn (fun () -> caml_dynamic_use_scope scope),
      spawn (fun () -> caml_dynamic_use_scope scope)
   in
   await l, await r

   Using the scope in child fibers causes them to inherit dynamic bindings
   on the path from the fork point to the root task, even if they're migrated
   to other worker threads. */
CAMLprim value caml_dynamic_freeze_scope(value unit);

/* Install [scope] as the current fiber's dynamic binding state and mark it as
   a concurrent task. See [caml_dynamic_freeze_scope] for usage. */
CAMLprim value caml_dynamic_use_scope(value scope);

=======
>>>>>>> main
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

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
