(** Translation environment for Flambda to Js_of_ocaml IR translation. *)

type t

(* CR selee: probably we will end up needing to store more info *)

(** Create a new environment. *)
val create : unit -> t

(** Map a Flambda2 continuation to a Jsir block address. *)
val add_block : t -> Flambda2_identifiers.Continuation.t -> Jsir.Addr.t -> t

(** Map a Flambda2 variable to a Jsir variable. *)
val add_var : t -> Flambda2_identifiers.Variable.t -> Jsir.Var.t -> t

(** Return the block address for the given continuation. Raises if given an
    unbound continuation. *)
val get_block_exn : t -> Flambda2_identifiers.Continuation.t -> Jsir.Addr.t

(** Return the Jsir variable for the given Flambda variable. Raises if given
    an unbound variable. *)
val get_var_exn : t -> Flambda2_identifiers.Variable.t -> Jsir.Var.t
