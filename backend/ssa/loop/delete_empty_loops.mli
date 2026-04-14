[@@@ocaml.warning "+a-40-41-42"]

(** Bypass every loop whose body does no observable work.

    A loop is considered empty when:
    - [Termination.analyze] reports [Terminates], and
    - [Dead_induction_var.analyze] reports [useless = true], i.e. every
      block parameter of the header is a basic induction variable whose
      only uses are its own update expression and the exit comparison.

    The transformation rewrites the loop header's [Branch] terminator into
    an unconditional [Goto] to the exit target. The loop body, back edges
    and IV update computations become unreachable; downstream CFG passes
    (DCE, unreachable-block removal, merge-block cleanup) are expected to
    clean them up. *)

type deletion =
  { loop : Induction_var.loop;
    exit_target : Ssa.block
  }

(** Mutates [t] in place and returns the list of loops whose header
    terminators were rewritten. *)
val run : Ssa.t -> deletion list

val print : Format.formatter -> deletion list -> unit
