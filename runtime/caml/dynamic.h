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

/* Pop a local binding for a dynamic variable.
   Must be paired with [caml_dynamic_push] on the same fiber. */
CAMLprim value caml_dynamic_pop(value dyn);

/* Return a handle (tagged pointer) to the current fiber, for use with
   [caml_dynamic_set_lexical_parent]. Unsafe: the handle is invalidated if
   the fiber's stack grows (reallocation frees the old block) or the fiber
   terminates. Callers must obtain it immediately before suspending (e.g.
   right before performing a fork effect), with no intervening OCaml calls
   that could trigger a stack check, and must not use it after the fiber
   resumes. */
CAMLprim value caml_dynamic_current_fiber(value unit);

/* Set (or, given Val_unit, clear) the lexical parent of the current fiber,
   making dynamic lookups see the bindings visible at [fiber] (its task's
   chain, up to the enclosing task base) before those of this fiber's own
   dynamic parent chain (see [dynamic_lookup] in dynamic.c).

   Intended for fork/join-style libraries pinning the scope of the fork
   point. The caller must guarantee:
   - This runs as the first action of a dedicated per-child fiber, never on
     a long-lived (e.g. scheduler worker) fiber, where the edge would
     outlive the task. The edge dies with the fiber.
   - The fork point's task span (fork point up to its task base) stays
     alive, with bindings and parent links unchanged, for as long as this
     fiber can run. Joining all children before the forking task unwinds
     provides liveness; children making their bindings in their own fibers,
     and effects from a child never being handled between the fork point
     and the task base, provide immutability. Handoff to and joining from
     another domain must synchronize (any scheduler queue does), making
     cross-domain reads of the span's tables safe.
   - This fiber must never escape into a continuation that outlives the
     join, or its edge dangles. Fibers created *within* it may escape
     freely: they carry no edge, so once their chain no longer passes
     through this fiber they resolve purely against their new parent chain.
   - Every chain reachable through a lexical edge ends at a fiber marked by
     [caml_dynamic_set_lexical_root] or at a parent link cut by
     continuation capture; an unmarked task base would let detours run into
     (and race with) the mounting worker's live chain. */
CAMLprim value caml_dynamic_set_lexical_parent(value fiber);

/* Mark the current fiber as the base of a scheduler task: lexical detours
   stop here instead of continuing into the scheduler's own chain (see
   [dynamic_lookup] in dynamic.c). The fiber's own lookups are unaffected.
   Schedulers must mark every task fiber they mount that is not itself a
   fork/join child, before running the task body. */
CAMLprim value caml_dynamic_set_lexical_root(value unit);


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
extern bool caml_dynamic_table_copy(dynamic_table_t dst, dynamic_table_t src);

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
