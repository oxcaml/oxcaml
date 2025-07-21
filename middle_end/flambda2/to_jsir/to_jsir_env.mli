(** Translation environment for Flambda to Js_of_ocaml IR translation. *)
type t

(* CR selee: probably we will end up needing to store more info *)

(** Create a new environment.

    [return_continuation] and [exn_continuation] refer to the top-level
    return/exception continuations, and does not change once the environment
    is created.
*)
val create :
  module_symbol:Symbol.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** Symbol corresponding to the module currently compiling. *)
val module_symbol : t -> Symbol.t

(** Map a Flambda2 continuation to a JSIR block address. *)
val add_continuation : t -> Continuation.t -> Jsir.Addr.t -> t

(** Map a Flambda2 variable to a JSIR variable. *)
val add_var : t -> Variable.t -> Jsir.Var.t -> t

(** Map a Flambda2 symbol to a JSIR variable. *)
val add_symbol : t -> Symbol.t -> Jsir.Var.t -> t

(** Set [var] to be an alias of [alias_of]. Raises if [alias_of] is not found
    in the environment. *)
val add_alias_of_var_exn : t -> var:Variable.t -> alias_of:Variable.t -> t

val add_alias_of_symbol_exn : t -> var:Variable.t -> alias_of:Symbol.t -> t

type continuation =
  | Return
  | Exception
  | Block of Jsir.Addr.t

(** Return the block address for the given continuation. Raises if given an
    unbound continuation. *)
val get_continuation_exn : t -> Continuation.t -> continuation

(** Return the Jsir variable for the given Flambda variable. Raises if given
    an unbound variable. *)
val get_var_exn : t -> Variable.t -> Jsir.Var.t

(** Return the Jsir variable for the given Flambda symbol. Raises if given
    an unbound symbol. *)
val get_symbol_exn : t -> Symbol.t -> Jsir.Var.t
