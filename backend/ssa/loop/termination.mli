[@@@ocaml.warning "+a-40-41-42"]

(** Simple loop-termination analysis driven by basic induction variables.

    For each loop, we examine its exit branch (restricted to a single-condition
    [Branch] terminator at the loop header where exactly one target is in the
    loop and the other is outside). If a basic induction variable is the
    comparison's IV-related operand, and the IV's monotonic direction makes
    the continue-condition eventually fail, the loop terminates. Otherwise the
    result is [Unknown]. *)

type t =
  | Terminates
  | Unknown

val analyze : Induction_var.loop -> Induction_var.biv list -> t

val print : Format.formatter -> (Induction_var.loop * t) list -> unit

type exit_branch =
  { condition : Ssa.instruction;
    continue_when_true : bool
  }

(** Identify the loop's exit branch when it has the simple shape the
    termination analysis recognises: a [Branch] terminator on the loop
    header, with exactly one condition arm, and exactly one of its two
    targets inside the loop body. Returns [None] otherwise. See the TODO
    next to the implementation in [termination.ml] for the precise list of
    cases that are intentionally not handled. *)
val find_exit_branch : Induction_var.loop -> exit_branch option
