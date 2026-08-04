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
     by [caml_dynamic_current_fiber]) stays alive, with the bindings this
     fiber may read unchanged, for as long as this fiber can run. Joining
     all children before the forking task unwinds provides liveness;
     freezing the fork point's newest table and bounding the child there
     (see [caml_dynamic_freeze] and [caml_dynamic_set_lexical_bound])
     provides immutability. Handoff to and joining from another domain
     must synchronize (any scheduler queue does), making cross-domain
     reads of the span's tables safe.
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

/* Freeze the newest table of the current fiber's chain, returning a handle
   (tagged pointer) to it. While frozen the table is immutable, so fibers
   pinned to this one may read it — and everything below it, frozen by
   construction — without synchronization. [caml_dynamic_push] never writes
   to a frozen table: the first binding pushed above a freeze allocates a
   fresh table on top of the chain (one allocation per outermost binding
   scope; nested scopes reuse it).

   fork/join calls this on the fork point before publishing a child that
   may run concurrently with this fiber (e.g. before running the other
   child inline as a plain call), and hands the returned handle to that
   child for [caml_dynamic_set_lexical_bound]. Freezes nest: a nested
   fork/join freezes the tables the inline child pushed, bounding its own
   children there, while the outer fork's children remain bounded below.

   Binding scopes opened before a freeze must not close until after the
   matching [caml_dynamic_unfreeze]; scopes opened after it must close
   before. (Both follow from with_temporarily scoping around fork/join.)
   May raise Out_of_memory. */
CAMLprim value caml_dynamic_freeze(value unit);

/* Undo the matching [caml_dynamic_freeze]. Called by fork/join after all
   children it published have been joined; freeze/unfreeze pairs nest
   LIFO on a fiber. */
CAMLprim value caml_dynamic_unfreeze(value unit);

/* Set (or, given Val_unit, clear) the newest table of the lexical parent's
   chain that detours through this fiber's edge may read. Pass the handle
   returned by the fork point's [caml_dynamic_freeze], alongside
   [caml_dynamic_set_lexical_parent]: the child then reads exactly the
   bindings that were visible when it was forked, and never races with
   bindings the forking fiber pushes while it runs. When unset, detours
   read the target's whole chain (only safe if the target cannot run,
   e.g. a fiber suspended by an effect). May raise Out_of_memory. */
CAMLprim value caml_dynamic_set_lexical_bound(value bound);


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

/* Hash table of local dynamic bindings, mapping dynamic ID to a stack of
   bindings. Each fiber's node holds a chain of these (see [dynamic_node_s]
   below): a base table plus heap tables pushed on top of frozen ones. */
typedef struct dynamic_table_s {
  size_t mask; /* capacity - 1 */
  size_t count;
  dynamic_stack_t bindings;

  /* Number of live freezes of this table (see [caml_dynamic_freeze]).
     The table is immutable while nonzero. */
  uintnat frozen;

  /* Next-older table of the owning fiber's chain, or NULL. The fiber's
     base table implicitly follows the oldest heap table. */
  struct dynamic_table_s *prev;
} dynamic_table_s, *dynamic_table_t;

/* Initialize a dynamic table to an empty state. */
extern void caml_dynamic_table_init(dynamic_table_t table);

/* Uninitialize a dynamic table, freeing any internal allocations. */
extern void caml_dynamic_table_free(dynamic_table_t table);

/* Duplicate a dynamic table (not its [prev] link, which is set to NULL).
   Returns false if allocation fails. */
extern bool caml_dynamic_table_copy(dynamic_table_t dst, dynamic_table_t src);

/* Allocate an empty heap dynamic table. Returns NULL if allocation fails. */
extern dynamic_table_t caml_dynamic_table_node_new(void);

/* Free a heap dynamic table and its contents. */
extern void caml_dynamic_table_node_delete(dynamic_table_t table);

/* A snapshot of a fiber's chain of dynamic tables, used to save and
   restore the binding state around callbacks (see caml_with_async_exns). */
typedef struct dynamic_saved_state_s {
  dynamic_table_s base;
  dynamic_table_t top; /* cloned heap chain, newest first, or NULL */
} dynamic_saved_state_s;

/* Snapshot [stack]'s chain into [out]. Returns false if allocation fails. */
extern bool caml_dynamic_state_save(struct stack_info *stack,
                                    dynamic_saved_state_s *out);

/* Replace [stack]'s chain with [saved], freeing the current chain.
   Ownership of [saved]'s contents transfers to the fiber. */
extern void caml_dynamic_state_restore(struct stack_info *stack,
                                       dynamic_saved_state_s *saved);

/* Register/unregister all bindings of a saved state as GC roots. */
extern void caml_dynamic_state_register_roots(dynamic_saved_state_s *state);
extern void caml_dynamic_state_unregister_roots(dynamic_saved_state_s *state);

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

   [top] is the newest heap table of the fiber's chain of tables, or NULL
   if the base [table] is the only one. Chained via [prev]; the base
   implicitly follows the oldest. Only the newest table can be frozen or
   unfrozen, since the chain only grows on top of frozen tables (see
   [caml_dynamic_freeze]).

   [lexical_bound] is the newest table of [lexical_parent]'s chain that
   lexical detours through this fiber's edge may read, or NULL to read the
   whole chain. Set (see [caml_dynamic_set_lexical_bound]) to the table
   frozen at the fork point, so this fiber neither sees nor races with
   bindings the forking fiber pushes while it runs.

   The GC scans each node's tables via its owning fiber only; edges between
   nodes are not scanned, and whoever installs an edge must keep the target
   span alive (see [caml_dynamic_set_lexical_parent]). */
typedef struct dynamic_node_s {
  dynamic_table_s table;
  dynamic_table_t top;
  struct dynamic_node_s* lexical_parent;
  struct dynamic_node_s* span_next;
  dynamic_table_t lexical_bound;
  bool lexical_root;
} dynamic_node_s, *dynamic_node_t;

/* Free a fiber's dynamic-binding state, if any. */
extern void caml_dynamic_node_free(struct stack_info* stack);

/* Apply a GC scanning action to all bindings in a node's chain. */
extern void caml_dynamic_node_scan_roots(dynamic_node_t node,
                                         scanning_action f,
                                         scanning_action_flags fflags,
                                         void *fdata);

#endif /* CAML_INTERNALS */

#endif /* CAML_DYNAMIC_H */
