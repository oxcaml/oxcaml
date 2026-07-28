[@@@ocaml.warning "+a-40-41-42"]

(** Detect basic induction variables that are "dead": ones whose only uses in
    the whole SSA graph are as arguments of their own back-edge update
    expression(s) and of the loop's exit comparison. Such IVs contribute no
    observable state beyond iteration and, given a proof of termination, can be
    replaced with their closed-form final value. A loop in which every header
    parameter is such a dead IV iterates without doing any observable work and
    can be removed entirely. *)

module Make (S : Ssa.Finished_graph) : sig
  type biv_result =
    { biv : Induction_var.Make(S).biv;
      dead : bool
    }

  type loop_result =
    { loop : Induction_var.Make(S).loop;
      bivs : biv_result list;
      useless : bool
    }

  (** [useless] is [true] iff every block parameter of the loop header was
      classified as a basic induction variable and each is dead. *)
  val analyze :
    (Induction_var.Make(S).loop * Induction_var.Make(S).biv list) list ->
    loop_result list

  (** [is_dead biv] is [true] iff [biv]'s only uses in the whole graph are the
      loop's exit comparison and its own back-edge update — i.e. it would become
      removable if the exit test stopped mentioning it. *)
  val is_dead : Induction_var.Make(S).biv -> bool

  val print : Format.formatter -> loop_result list -> unit
end
