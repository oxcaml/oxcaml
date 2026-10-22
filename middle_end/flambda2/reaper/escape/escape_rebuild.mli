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

(** The rebuilding side of the escape analysis (see [Escape_analysis]): turning
    the allocations it found to be local into stack allocations, inserting the
    regions they live in, and changing the calling convention of the functions
    made to return at mode local. These functions are called from [Rebuild]. *)

(** ['r] is the type of the result accumulated by [Rebuild], through which the
    continuations closing regions are threaded (see [closers]). *)
type 'r t

(** Continuations closing the regions left by a jump from a [Switch] arm or a
    non-tail application, shared by all such jumps of a region and bound at the
    start of its scope. *)
type closers

val no_closers : closers

(** Statistics on the heap allocations rebuilt (blocks, boxed numbers and sets
    of closures): how many were made local and how many stayed on the heap, with
    their sizes in words. Printed by [report_stats] with the reaper debug flag
    [escape-stats] (one line per compilation unit) or [escape-stats-verbose]
    (also one line per allocation); see [escape_rebuild.ml] for the format. *)
type stats

val empty_stats : stats

val report_stats : _ t -> stats -> unit

val create :
  solution:Escape_solution.t ->
  code_ids_directly_called:(Name.t -> Code_id.Set.t Or_unknown.t) ->
  params_of_continuation:(Continuation.t -> Bound_parameter.t list) ->
  conts_per_scope:Continuation.Set.t Code_id_or_name.Map.t ->
  continuation_info:Traverse_acc.continuation_info Continuation.Map.t ->
  machine_width:Target_system.Machine_width.t ->
  return_continuation:Continuation.t ->
  closers_of:('r -> closers) ->
  with_closers:(closers -> 'r -> 'r) ->
  'r t

(** The allocation mode for the block of the given kind, with the given number
    of fields, bound to the given variable: local in the innermost region if the
    analysis decided so. The allocation is recorded in the statistics. *)
val localize_make_block :
  _ t ->
  stats ->
  Bound_var.t ->
  Flambda_primitive.Block_kind.t ->
  Alloc_mode.For_allocations.t ->
  num_fields:int ->
  dbg:Debuginfo.t ->
  Alloc_mode.For_allocations.t * stats

(** The same for a set of closures bound to the given variables; [code_metadata]
    gives the metadata of the code of its functions. *)
val localize_set_of_closures :
  _ t ->
  stats ->
  Bound_var.t list ->
  Flambda.Set_of_closures.t ->
  code_metadata:(Code_id.t -> Code_metadata.t) ->
  Alloc_mode.For_allocations.t ->
  Alloc_mode.For_allocations.t * stats

(** Make a [Box_number] bound to the given variable allocate locally if the
    analysis decided so (other defining expressions are left unchanged), and
    record it in the statistics. *)
val localize_box_number :
  _ t -> stats -> Bound_var.t -> Flambda.Named.t -> Flambda.Named.t * stats

(** Close the regions left by a jump to the given continuation, before the given
    expression (which performs the jump). *)
val end_regions_before_jump :
  _ t -> Continuation.t -> Rebuilt_expr.t -> Rebuilt_expr.t

(** Redirect the arms of a [Switch] jumping to continuations that leave regions
    through closing continuations. *)
val redirect_switch_arms :
  _ t ->
  closers ->
  Switch_arm.t Target_ocaml_int.Map.t ->
  Switch_arm.t Target_ocaml_int.Map.t * closers

(** Rebuild an application with [rebuild], after giving it a local return mode
    if the callee was made to return at mode local, and close the regions left
    when it returns. *)
val rebuild_apply :
  _ t ->
  closers ->
  Flambda.Apply.t ->
  rebuild:(Flambda.Apply.t -> Rebuilt_expr.t) ->
  Rebuilt_expr.t * closers

(** Rebuild the handler of a recursive [Let_cont] forming the given region scope
    (see [Rev_expr.Let_cont_rec.region_scope]), adding a region around it if
    needed. *)
val rebuild_region_scope :
  'r t ->
  scope:Code_id_or_name.t ->
  rebuild_body:('r t -> Rebuilt_expr.t * 'r) ->
  Rebuilt_expr.t * 'r

(** Enter the body of a function: returns its allocation mode, made
    [Maybe_alloc_stack] if the function is to return at mode local. *)
val enter_function :
  'r t ->
  code_id:Code_id.t ->
  return_continuation:Continuation.t ->
  my_alloc_mode:Alloc_mode.For_applications.t ->
  Alloc_mode.For_applications.t * 'r t

(** Rebuild the body of a function (entered with [enter_function]), adding a
    region around it if needed. *)
val rebuild_function_body :
  'r t ->
  code_id:Code_id.t ->
  rebuild_body:('r t -> Rebuilt_expr.t * 'r) ->
  Rebuilt_expr.t * 'r
