[@@@ocaml.warning "+a-30-40-41-42"]

val run : Cfg_with_infos.t -> Cfg_with_infos.t * Regalloc_rematerializations.t

module For_testing : sig
  val rounds : int ref
end
