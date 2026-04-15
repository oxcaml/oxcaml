[@@@ocaml.warning "+a-30-40-41-42"]

(* Returns `None` if the functions is deemed too complex for IRC allocation. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t option

module For_testing : sig
  val rounds : int ref
end
