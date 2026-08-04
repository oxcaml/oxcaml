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

struct stack_info;

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

/* Return a handle (tagged pointer) to the current fiber's dynamic scope —
   its stable [dynamic_node_s], see below — for use with
   [caml_dynamic_set_lexical_parent], and freeze the span links from this
   fiber up to its task base. The handle stays valid however the fiber's
   stack grows or moves; it must not be used after the fiber terminates.

   The task base must be identifiable when the handle is taken: the freeze
   stops at the first fiber that is marked as a lexical root (see
   [caml_dynamic_set_lexical_root]), carries a lexical edge of its own, or
   has no parent. Fork/join libraries must take the handle at the fork
   point, before publishing it to children. May raise Out_of_memory. */
CAMLprim value caml_dynamic_current_fiber(value unit);

/* Set (or, given Val_unit, clear) the lexical parent of the current fiber,
   making dynamic lookups see the bindings visible at the fork point whose
   handle is given (its task's fibers up to the enclosing task base, as
   frozen when the handle was taken) before those of this fiber's own
   dynamic parent chain (see [dynamic_lookup] in dynamic.c).

   Intended for fork/join-style libraries pinning the scope of the fork
   point. The caller must guarantee:
   - This runs as the first action of a dedicated per-child fiber, never on
     a long-lived (e.g. scheduler worker) fiber, where the edge would
     outlive the task. The edge dies with the fiber.
   - The fork point's task span (fork point up to its task base, as frozen
     by [caml_dynamic_current_fiber]) stays alive, with bindings unchanged,
     for as long as this fiber can run. Joining all children before the
     forking task unwinds provides liveness; children making their bindings
     in their own fibers provides immutability. Handoff to and joining from
     another domain must synchronize (any scheduler queue does), making
     cross-domain reads of the span's tables safe.
   - This fiber must never escape into a continuation that outlives the
     join, or its edge dangles. Fibers created *within* it may escape
     freely: they carry no edge, so once their chain no longer passes
     through this fiber they resolve purely against their new parent chain.
   - Every task base is marked by [caml_dynamic_set_lexical_root] before
     the task body runs; an unmarked base would let
     [caml_dynamic_current_fiber] freeze the detour into (and race with)
     the mounting worker's chain. */
CAMLprim value caml_dynamic_set_lexical_parent(value fiber);

/* Mark the current fiber as the base of a scheduler task: span freezing
   and lexical detours stop here instead of continuing into the scheduler's
   own chain (see [caml_dynamic_current_fiber] and [dynamic_lookup] in
   dynamic.c). The fiber's own lookups are unaffected. Schedulers must mark
   every task fiber they mount that is not itself a fork/join child, before
   running the task body. */
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


/* Per-fiber dynamic-binding state. Allocated lazily, owned by exactly one
   fiber ([stack_info.dyn_node]) and freed with it, but allocated separately
   from the stack block: stack reallocation moves [stack_info], not this
   node, so lexical edges and the handles returned by
   [caml_dynamic_current_fiber] stay valid for the fiber's whole lifetime.

   [lexical_parent] is the fork edge: the node of the fiber whose dynamic
   bindings lexically enclose this fiber's, or NULL. Set on fork/join child
   tasks (see [caml_dynamic_set_lexical_parent]); dynamic lookups detour
   through it before falling back to the fiber's own parent chain, so
   children see the bindings visible at the fork point even if a scheduler
   runs them under an unrelated parent.

   [span_next] is the frozen span link: the node of the enclosing fiber of
   the same task, or NULL at the task base. Frozen by
   [caml_dynamic_current_fiber] while the fork point's chain is safe to
   walk; detours follow it instead of live parent links, whose owning
   [stack_info]s may be reallocated concurrently by the domain running the
   fork point.

   [lexical_root] marks the base fiber of a scheduler task: span freezing
   (and hence detours) stop here instead of continuing into the scheduler's
   own chain. The fiber's own lookups ignore it and fall through into the
   executing worker's chain. Set by schedulers (see
   [caml_dynamic_set_lexical_root]).

   The GC scans each node's table via its owning fiber only; edges between
   nodes are not scanned, and whoever installs an edge must keep the target
   span alive (see [caml_dynamic_set_lexical_parent]). */
typedef struct dynamic_node_s {
  dynamic_table_s table;
  struct dynamic_node_s* lexical_parent;
  struct dynamic_node_s* span_next;
  bool lexical_root;
} dynamic_node_s, *dynamic_node_t;

/* Free a fiber's dynamic-binding state, if any. */
extern void caml_dynamic_node_free(struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
