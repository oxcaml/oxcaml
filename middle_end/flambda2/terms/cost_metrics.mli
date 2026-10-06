(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

val zero : t

val from_size : Code_size.t -> t

(** Add the cost of the prologue and epilogue of a function whose body has the
    given metrics; see [Code_size.add_function_frame]. *)
val add_function_frame : t -> t

val size : t -> Code_size.t

val removed : t -> Removed_operations.t

val print : Format.formatter -> t -> unit

(** The metrics of two pieces of code whose relative placement is unknown. *)
val ( + ) : t -> t -> t

(** [seq a b] are the metrics of the code [a] followed by the code [b]; see
    [Code_size.seq]. *)
val seq : t -> t -> t

(** The metrics of the code [t] together with code placed elsewhere; see
    [Code_size.with_out_of_line]. *)
val with_out_of_line : t -> out_of_line:t -> t

type code_characteristics =
  { cost_metrics : t;
    function_slot_size : int
  }

val set_of_closures :
  find_code_characteristics:(Code_id.t -> code_characteristics) ->
  Set_of_closures.t ->
  t

val increase_due_to_let_expr :
  is_phantom:bool -> cost_metrics_of_defining_expr:t -> t

val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t

val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t

val notify_added : code_size:Code_size.t -> t -> t

val notify_removed : operation:Removed_operations.t -> t -> t

val evaluate : args:Inlining_arguments.t -> t -> float

(** The size less the bonus for the removed operations: what the ratio criterion
    judges, before the call-site credit. *)
val adjusted_size : t -> float

(** What these metrics cost against a speculative inlining budget: [evaluate]
    under the threshold criterion, [adjusted_size] under the ratio one. *)
val budget_charge : args:Inlining_arguments.t -> t -> float

(** The credit, under the current criterion, for code of this cost removed
    outside the inlined body. *)
val credit : args:Inlining_arguments.t -> t -> float

val equal : t -> t -> bool
