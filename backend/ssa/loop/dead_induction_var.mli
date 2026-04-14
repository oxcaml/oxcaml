[@@@ocaml.warning "+a-40-41-42"]

(** Detect basic induction variables that are "dead": ones whose only uses
    in the whole SSA graph are as arguments of their own back-edge update
    expression(s) and of the loop's exit comparison. Such IVs contribute no
    observable state beyond iteration and, given a proof of termination,
    can be replaced with their closed-form final value. A loop in which
    every header parameter is such a dead IV iterates without doing any
    observable work and can be removed entirely. *)

type biv_result =
  { biv : Induction_var.biv;
    dead : bool
  }

type loop_result =
  { loop : Induction_var.loop;
    bivs : biv_result list;
    useless : bool
  }

(** [useless] is [true] iff every block parameter of the loop header was
    classified as a basic induction variable and each is dead. *)
val analyze :
  Ssa.t ->
  (Induction_var.loop * Induction_var.biv list) list ->
  loop_result list

val print : Format.formatter -> loop_result list -> unit
