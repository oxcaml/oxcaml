[@@@ocaml.warning "+a-30-40-41-42"]

(** Returns [Error prepared] if the function is too complex for IRC. No
    allocation rewrites have occurred, so [prepared] can be passed directly to
    [Regalloc_ls.run_after_prelude]. *)
val run :
  Cfg_with_infos.t ->
  ( Cfg_with_infos.t * Regalloc_rematerializations.t,
    Regalloc_rewrite.prelude_result )
  result

module For_testing : sig
  val rounds : int ref
end
