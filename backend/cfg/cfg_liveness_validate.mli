[@@@ocaml.warning "+a-40-41-42"]

(** Checks liveness sets against an independent Datalog specification. Z3 is
    used only to diagnose disagreements. *)

val validate_liveness : Cfg.t -> Cfg_liveness.domain InstructionId.Tbl.t -> unit
