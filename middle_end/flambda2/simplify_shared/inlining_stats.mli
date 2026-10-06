(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Recording of the per-compilation-unit statistics about inlining printed by
    [-dinlining-stats] (see [Inlining_stats_table], which the backend completes
    and prints at the end of the unit). Nothing is computed when the flag is
    off. *)

val enabled : unit -> bool

type pass =
  | Simplify
  | Closure_conversion

(** A decision at a call site whose callee is known. [callee_size] is the size
    of the callee's code before any simplification. *)
val record_call_site_decision :
  pass:pass ->
  in_speculation:bool ->
  is_a_functor:bool ->
  callee_size:Code_size.t ->
  apply:Apply_expr.t ->
  Call_site_inlining_decision_type.t ->
  unit

(** A call site whose callee is unknown. *)
val record_unknown_callee :
  pass:pass -> in_speculation:bool -> Apply_expr.t -> unit

(** The inlining decision taken for a function definition. *)
val record_function_definition :
  pass:pass ->
  in_speculation:bool ->
  code_metadata:Code_metadata.t ->
  Function_decl_inlining_decision_type.t ->
  unit

(** Run a speculative inlining, recording the time it took. *)
val time_speculation : outermost:bool -> (unit -> 'a) -> 'a

(** Events of the speculative inlining budget; [in_region] is [true] for a
    region of an inlined body and [false] during a speculation. *)
val record_budget_opened : in_region:bool -> budget:float -> unit

val record_budget_charge :
  in_region:bool ->
  charge:float ->
  credit_granted:float ->
  credit_capped:float ->
  credit_used:float ->
  exhausted:bool ->
  unit

(** Statistics about the shape of the final code of the unit. *)
val record_final_unit :
  machine_width:Target_system.Machine_width.t -> Flambda_unit.t -> unit
