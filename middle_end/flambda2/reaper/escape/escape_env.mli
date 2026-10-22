(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The part of the traversal environment ([Traverse_env]) used by the escape
    analysis: where heap allocations at the current point may be turned into
    stack allocations (see [Escape_analysis]). *)

(** - [No_region_scope]: nowhere in the current function, not even in recursive
      continuation handlers. This is used when the analysis is disabled, and for
      functions that may allocate their result in the caller's region (such
      allocations must not happen while an inner region is open).

    - [Toplevel]: not at the current point, but recursive continuation handlers
      may open their own region scopes. Used for the toplevel of the unit.

    - [Scope scope]: allocations at the current point are candidates, in the
      region scope named by [scope]: the code id of the enclosing function, or a
      dummy variable naming a recursive [Let_cont] whose handlers get a region
      each (so that a function-level region does not grow with every iteration).
*)
type region_scope =
  | No_region_scope
  | Toplevel
  | Scope of Code_id_or_name.t

type t

(** The environment at the start of a function body (or of the unit), with its
    return and exception continuations. *)
val create :
  region_scope:region_scope ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** The region scope in which allocations at the current point are candidates
    for stack allocation, if any. There is none while a region pre-existing in
    the term (see [enter_foreign_region]) is open, since allocations would go to
    that region instead. *)
val region_scope : t -> Code_id_or_name.t option

(** All region scopes enclosing the current point, innermost first. *)
val enclosing_region_scopes : t -> Code_id_or_name.t list

(** The return continuation of the enclosing function (or of the unit, at
    toplevel): applications returning to it are tail calls. *)
val return_continuation : t -> Continuation.t

(** The exception continuation of the enclosing function (or of the unit). *)
val exn_continuation : t -> Continuation.t

(** Enter the handlers of a recursive [Let_cont] whose region scope is named by
    [scope]. *)
val enter_recursive_handler : t -> scope:Variable.t -> t

(** Note that the given region, defined by a [Begin_region] primitive
    pre-existing in the term (as inserted for [local_] allocations), is open at
    the current point. *)
val enter_foreign_region : t -> Variable.t -> t

(** Note that the given region has been closed by an [End_region] primitive. *)
val exit_foreign_region : t -> Variable.t -> t

(** Note that the given try-region, defined by a [Begin_try_region] primitive,
    is open at the current point. Unlike the regions above, try-regions are not
    closed on the normal path, so allocations made while they are open belong to
    the enclosing region and remain candidates. *)
val enter_try_region : t -> Variable.t -> t

(** Note that the given try-region has been closed by an [End_try_region]
    primitive. *)
val exit_try_region : t -> Variable.t -> t

(** Enter the handler of a continuation with the given parameters. *)
val enter_handler : t -> params:Variable.t list -> t

(** The parameters of the enclosing continuation handlers that were defined
    while the given try-region was open: closing that try-region frees
    everything allocated since it was opened, so no such value may be among the
    arguments passed to these handlers. *)
val handler_params_freed_by_end_of_try_region :
  t -> Variable.t -> Variable.t list
