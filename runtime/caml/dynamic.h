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

#include <stdbool.h>
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

/* Freeze dynamic binding state between the current fiber and the root task,
   returning an identifying handle.

   The following functions are intended to be used by structured concurrency
   primitives in the following order:

   caml_dynamic_set_root ();
   let scope = caml_dynamic_freeze_scope () in
   let l, r =
      spawn (fun () -> caml_dynamic_use_scope scope),
      spawn (fun () -> caml_dynamic_use_scope scope)
   in
   let res = await l, await r in
   caml_dynamic_thaw_scope scope;
   res

   Using the scope in child fibers causes them to inherit dynamic bindings
   installed on the path to the root task, even if they're migrated to other
   worker threads.

   All calls to [caml_dynamic_freeze_scope] must occur in a descendant of
   a fiber that called [caml_dynamic_set_root]. The returned [scope] must
   outlive all child fibers that call [caml_dynamic_use_scope], the parent
   must not perform an effect during the lifetime of [scope], and [scope]
   must be passed to [caml_dynamic_thaw_scope] after all children exit. */
CAMLprim value caml_dynamic_freeze_scope(value unit);

/* Make a set of frozen dynamic bindings visible to the current fiber.
   See [caml_dynamic_freeze_scope] for usage. */
CAMLprim value caml_dynamic_use_scope(value scope);

/* Release a scope returned by [caml_dynamic_freeze_scope]. Must be called on
   the fiber that froze the scope and the scope must not be in use. */
CAMLprim value caml_dynamic_thaw_scope(value scope);

/* Mark the current fiber as a root task. Dynamic bindings installed above
   the current fiber will not be captured by [caml_dynamic_freeze_scope].
   See [caml_dynamic_freeze_scope] for usage. */
CAMLprim value caml_dynamic_set_root(value unit);

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
  struct dynamic_table_s* parent;

  /* Number of frozen scopes that refer to this table.
     This table is immutable if [frozen] > 0. Attempting to change the binding
     state of a frozen table instead allocates a fresh child table. */
  uintnat frozen;
} dynamic_table_s, *dynamic_table_t;

/* Freeze and return the current table. Changes to the binding state
   after [snapshot] will not be visible after [restore]. */
extern dynamic_table_t caml_dynamic_state_snapshot(void);

/* Restore the current table to its state at [snapshot], freeing any
   intermediate tables installed in the interim. */
extern void caml_dynamic_state_restore(dynamic_table_t snapshot);

/* Per-fiber binding state. Owned by the fiber, but separately allocated for
   stability (the stack_info allocation may be resized). Scanned by the GC via
   the owning fiber only.

   [table] stores dynamic binding state at the base of the owning fiber.
   Its chain of parent pointers records the path to the root task.

   [newest] typically points to [table], unless [table] was frozen and another
   binding was installed. In this case, [newest] points to a separately allocated
   table whose parent pointer records the path to [table] (then to the root).

   [is_task] indicates whether this node is a concurrent task. If it is, lookup
   proceeds into [table.parent], which points to the inherited binding state. */
typedef struct dynamic_node_s {
  dynamic_table_s table;
  dynamic_table_t newest;
  bool is_task;
} dynamic_node_s, *dynamic_node_t;

/* Free a dynamic node and its contents. */
extern void caml_dynamic_node_free(dynamic_node_t node);

/* Apply a GC scanning action to all bindings in a node's chain. */
extern void caml_dynamic_node_scan_roots(dynamic_node_t node,
                                         scanning_action f,
                                         scanning_action_flags fflags,
                                         void *fdata);

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
