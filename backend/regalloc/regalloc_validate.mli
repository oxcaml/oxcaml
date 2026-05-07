[@@@ocaml.warning "+a-30-40-41-42"]

module Description : sig
  type t

  val create : Cfg_with_layout.t -> t option
end

module Error : sig
  type t

  val print : Format.formatter -> t -> unit
end

val test :
  Description.t -> Cfg_with_layout.t -> (Cfg_with_layout.t, Error.t) Result.t

val run : Description.t option -> Cfg_with_infos.t -> Cfg_with_infos.t

(** Record the pre-allocation shape (description-style abstract registers and
    desc) of a rematerialized basic instruction added by the split pass. The
    validator looks this up when processing the instruction in the
    post-allocation CFG, so that the equation transfer uses the same abstract
    register stamps as the existing description-based equations rather than the
    fresh post-split stamps. Should be called for every basic instruction that
    [Regalloc_split.RewriteAsRematerialize] inserts.

    Argument [desc] is preserved verbatim. [arg]/[res] must be the pre-split
    (description-style) abstract registers — i.e. without locations on Named
    registers. *)
val record_rematerialization :
  id:InstructionId.t ->
  desc:Cfg_intf.S.basic ->
  arg:Reg.t array ->
  res:Reg.t array ->
  unit

(** Reset the rematerialization side-table. [Description.create] calls this
    automatically; the call is also exposed in case a caller wants to discard
    stale state explicitly (e.g. between unrelated compilation units). *)
val clear_rematerializations : unit -> unit
