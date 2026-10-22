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

(** The allocation mode for the block bound to the given variable (resp. bound
    pattern, sets of closures): local in the innermost region if the analysis
    decided so. *)
val localize_alloc_mode :
  _ t ->
  Variable.t ->
  Alloc_mode.For_allocations.t ->
  Alloc_mode.For_allocations.t

val localize_alloc_mode_of_pattern :
  _ t ->
  Bound_pattern.t ->
  Alloc_mode.For_allocations.t ->
  Alloc_mode.For_allocations.t

val localize_set_of_closures_alloc_mode :
  _ t ->
  Bound_var.t list ->
  Alloc_mode.For_allocations.t ->
  Alloc_mode.For_allocations.t

(** Make a [Box_number] bound to the given variable allocate locally if the
    analysis decided so (other defining expressions are left unchanged). *)
val localize_box_number :
  _ t -> Bound_var.t -> Flambda.Named.t -> Flambda.Named.t

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
