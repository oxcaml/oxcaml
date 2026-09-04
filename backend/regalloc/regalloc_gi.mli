[@@@ocaml.warning "+a-30-40-41-42"]

val run :
  description:Regalloc_validate.Description.t option ->
  Cfg_with_infos.t ->
  Cfg_with_infos.t

module For_testing : sig
  val rounds : int ref
end
