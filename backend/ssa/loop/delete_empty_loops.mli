[@@@ocaml.warning "+a-40-41-42"]

(** Bypass every loop whose body does no observable work.

    A loop is considered empty when:
    - [Termination.analyze] reports [Terminates], and
    - [Dead_induction_var.analyze] reports [useless = true], i.e. every block
      parameter of the header is a basic induction variable whose only uses are
      its own update expression and the exit comparison.

    The transformation rewrites the loop header's [Branch] terminator into an
    unconditional [Goto] to the exit target. The loop body, back edges and IV
    update computations become unreachable; downstream CFG passes (DCE,
    unreachable-block removal, merge-block cleanup) are expected to clean them
    up. *)

module Make (S : Ssa.Finished_graph) : sig
  type deletion =
    { loop : Induction_var.Make(S).loop;
      exit_target : S.Block.t
    }

  (** Mutates the graph in place and returns the list of loops whose header
      terminators were rewritten. *)
  val run : unit -> deletion list

  val print : Format.formatter -> deletion list -> unit
end

(** Delete empty loops in [input], returning a fresh finished graph (rebuilt so
    the stale metadata and now-unreachable loop bodies are cleaned up) together
    with the number of loops deleted. Returns [input] unchanged when there is
    nothing to delete. *)
val run : (module Ssa.Finished_graph) -> (module Ssa.Finished_graph) * int
