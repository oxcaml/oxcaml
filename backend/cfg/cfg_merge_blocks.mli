[@@@ocaml.warning "+a-40-41-42"]

(* This pass merges CFG blocks deemed equivalent, thus deleting duplicates. This
   is different from the kind of merge implemented in `Cfg_simplify`, which is
   about concatenating blocks in a straight line. *)

val run_before_register_allocation : Cfg_with_layout.t -> Cfg_with_layout.t

val run_after_register_allocation : Cfg_with_layout.t -> Cfg_with_layout.t
