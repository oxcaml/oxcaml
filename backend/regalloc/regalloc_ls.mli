[@@@ocaml.warning "+a-30-40-41-42"]

val run : Cfg_with_infos.t -> Cfg_with_infos.t * Regalloc_rematerializations.t

(** Allocates an already prepared CFG, including on IRC fallback. Does not reset
    register locations or rerun splitting. *)
val run_after_prelude :
  Regalloc_rewrite.prelude_result ->
  Cfg_with_infos.t * Regalloc_rematerializations.t

module For_testing : sig
  val rounds : int ref
end
