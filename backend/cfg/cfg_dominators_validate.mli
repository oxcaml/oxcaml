[@@@ocaml.warning "+a-40-41-42"]

(** Checks immediate dominators against an independent Datalog specification. Z3
    is used only to diagnose disagreements. *)

val validate_idom : Cfg.t -> Label.t Label.Tbl.t -> unit
