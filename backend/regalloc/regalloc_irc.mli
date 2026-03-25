[@@@ocaml.warning "+a-30-40-41-42"]

(* XXX/doc *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t option

module For_testing : sig
  val rounds : int ref
end
