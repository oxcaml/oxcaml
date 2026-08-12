[@@@ocaml.warning "+a-40-41-42"]

(** Facts extracted from CFGs for independent validation of compiler analyses.
    They are also used to produce diagnostic Z3 reproducers when validation
    fails. *)

module Graph : sig
  type t =
    { entry : Label.t;
      nodes : Label.t list;
      edges : (Label.t * Label.t) list
    }

  val create : Cfg.t -> t
end
