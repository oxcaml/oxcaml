[@@@ocaml.warning "+a-40-41-42"]

(** Simple loop-termination analysis driven by basic induction variables.

    For each loop, we examine its exit branch (restricted to a [Branch]
    terminator at the loop header where exactly one target is in the loop and
    the other is outside). If a basic induction variable is the comparison's
    IV-related operand, and the IV's monotonic direction makes the
    continue-condition eventually fail, the loop terminates. Otherwise the
    result is [Unknown]. *)

module Make (S : Ssa.Finished_graph) : sig
  type t =
    | Terminates
    | Unknown

  val analyze :
    Induction_var.Make(S).loop -> Induction_var.Make(S).biv list -> t

  val print : Format.formatter -> (Induction_var.Make(S).loop * t) list -> unit

  type exit_branch =
    { condition : S.Instruction.t;
      continue_when_true : bool
    }

  (** Identify the loop's exit branch when it has the simple shape the
      termination analysis recognises: a [Branch] terminator on the loop header
      with exactly one of its two targets inside the loop body. Returns [None]
      otherwise. *)
  val find_exit_branch : Induction_var.Make(S).loop -> exit_branch option
end
