[@@@ocaml.warning "+a-30-40-41-42"]

module Description : sig
  type t

  val create : Cfg_with_layout.t -> t option
end

module Error : sig
  type t

  val print : Format.formatter -> t -> unit
end

(** [test description ~rematerializations cfg] validates the allocated [cfg]
    against the pre-allocation [description]; [rematerializations] are the
    instructions the allocator reports having inserted as rematerializations
    (cf. [Regalloc_rematerializations]). *)
val test :
  Description.t ->
  rematerializations:Regalloc_rematerializations.t ->
  Cfg_with_layout.t ->
  (Cfg_with_layout.t, Error.t) Result.t

val run :
  Description.t option ->
  rematerializations:Regalloc_rematerializations.t ->
  Cfg_with_infos.t ->
  Cfg_with_infos.t
