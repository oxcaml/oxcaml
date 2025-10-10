[@@@ocaml.warning "+a-40-41-42"]

(** Merge blocks and eliminate dead blocks  *)
val run : Cfg_with_layout.t -> is_after_regalloc:bool -> Cfg_with_layout.t
