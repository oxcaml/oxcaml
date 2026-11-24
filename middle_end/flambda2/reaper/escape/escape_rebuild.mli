(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(** The rebuilding side of the escape analysis (see [Escape_analysis]): turning
    the allocations it found to be local into stack allocations, inserting the
    regions they live in, and changing the calling convention of the functions
    made to return at mode local. These functions are called from [Rebuild].

    When the analysis has not been run (see [create]), all of them leave the
    terms, modes and metadata they are given unchanged. *)

type t

(** The state for rebuilding a compilation unit. The analysis has been run if
    the graph has been solved (see [Escape_graph.solved]).

    - [is_unboxed] tells which allocations the reaper unboxes, and
      [allocation_is_kept] which variables bound to allocations it keeps (the
      latter is only used for the statistics);
    - [code_ids_directly_called] is [Analysis.code_id_actually_directly_called];
    - [get_code_metadata] gives the metadata of any code (only used for the
      statistics). *)
val create :
  Escape_graph.t ->
  is_unboxed:(Code_id_or_name.t -> bool) ->
  allocation_is_kept:(Variable.t -> bool) ->
  code_ids_directly_called:(Name.t -> Code_id.Set.t Or_unknown.t) ->
  get_code_metadata:(Code_id.t -> Code_metadata.t) ->
  continuation_info:Traverse_acc.Continuation_info.t Continuation.Map.t ->
  machine_width:Target_system.Machine_width.t ->
  t

(** What rebuilding accumulates, to be threaded through it: the statistics, and
    the continuations created by [redirect_switch_arms] and [rebuild_apply],
    until they are bound by [leave_recursive_handler] or [leave_function]. *)
type acc

val empty_acc : acc

(** Print the statistics on the heap allocations seen by [localize] (blocks,
    boxed numbers and sets of closures): how many were made local and how many
    stayed on the heap, with their sizes in words. Nothing is printed unless the
    reaper debug flag [escape-stats] (one line per compilation unit) or
    [escape-stats-verbose] (also one line per allocation) is given; see
    [escape_rebuild.ml] for the format. *)
val report_stats : t -> acc -> unit

(** The defining expression of a [Let], to be rebuilt in place of the given one:
    a heap allocation (of a block, boxed number or set of closures) that the
    analysis decided to make local allocates in the innermost region. *)
val localize :
  t -> acc -> Bound_pattern.t -> Rev_expr.rev_named -> Rev_expr.rev_named * acc

(** See [Escape_solution.adjust_code_metadata]. *)
val adjust_code_metadata : t -> Code_id.t -> Code_metadata.t -> Code_metadata.t

(** Close the regions left by a jump to the given continuation, before the given
    expression (which performs the jump). *)
val end_regions_before_jump :
  t -> Continuation.t -> Rebuilt_expr.t -> Rebuilt_expr.t

(** Redirect the arms of a [Switch] jumping to continuations that leave regions
    through continuations closing these regions. [params_of_continuation] must
    give fresh parameters for the continuation, as it is rebuilt. *)
val redirect_switch_arms :
  t ->
  acc ->
  params_of_continuation:(Continuation.t -> Bound_parameter.t list) ->
  Switch_arm.t Target_ocaml_int.Map.t ->
  Switch_arm.t Target_ocaml_int.Map.t * acc

(** Rebuild an application with [rebuild], after giving it a local return mode
    if the callee was made to return at mode local, and close the regions left
    when it returns. *)
val rebuild_apply :
  t ->
  acc ->
  params_of_continuation:(Continuation.t -> Bound_parameter.t list) ->
  Flambda.Apply.t ->
  rebuild:(Flambda.Apply.t -> Rebuilt_expr.t) ->
  Rebuilt_expr.t * acc

(** The state for rebuilding the handler of the given continuation. The handler
    of a recursive continuation may get a region of its own (see
    [Escape_graph.recursive_scope]). *)
val enter_handler : t -> Continuation.t -> t

(** To be called on the rebuilt handler of a continuation, with the state
    returned by [enter_handler]: adds what opens and closes regions at its
    start. *)
val leave_handler :
  t -> acc -> Continuation.t -> Rebuilt_expr.t -> Rebuilt_expr.t * acc

(** The state for rebuilding the given body of a function, and its allocation
    mode, made [Maybe_alloc_stack] if the function is to return at mode local.
    The body gets a region if it has local allocations; for a function returning
    at mode local, see [Escape_exclave]. *)
val enter_function :
  t ->
  code_id:Code_id.t ->
  return_continuation:Continuation.t ->
  my_alloc_mode:Alloc_mode.For_applications.t ->
  Rev_expr.t ->
  Alloc_mode.For_applications.t * t

(** To be called on the rebuilt body of a function, with the state returned by
    [enter_function]: adds the region around it if needed. *)
val leave_function :
  t -> acc -> code_id:Code_id.t -> Rebuilt_expr.t -> Rebuilt_expr.t * acc
