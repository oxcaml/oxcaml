open Datalog_imports

type 'a variable

module Variable : sig
  type 'a t = 'a variable

  val print : Format.formatter -> 'a t -> unit

  val create : string -> 'a t

  val name : 'a t -> string

  include Heterogenous_list.S with type 'a t := 'a t

  module Id : sig
    type t

    val equal : t -> t -> bool

    val hash : t -> int

    module Set : Container_types.Set with type elt = t

    module Map : Container_types.Map with type key = t

    module Tbl : Hashtbl.S with type key = t
  end

  val uid : 'a t -> Id.t

  val provably_equal : 'a t -> 'b t -> ('a, 'b) Type.eq option

  (* Raises [Misc.Fatal_error] if the two variables are not equal.

     Variables with the same [uid] are guaranteed to be equal. *)
  val must_be_equal : 'a t -> 'b t -> ('a, 'b) Type.eq
end

type 'a term =
  | Variable of 'a variable
  | Literal of 'a

val print_term :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a term -> unit

val var : 'a variable -> 'a term

val lit : 'a -> 'a term

module Term : sig
  type 'a t = 'a term

  include Heterogenous_list.S with type 'a t := 'a t
end

type ('k, 'v) relation =
  | Table : (_, 'k, 'v) Table.Id.t -> ('k, 'v) relation
  | Unless : (_, 'k, 'v) Table.Id.t -> ('k, unit) relation
  | Distinct : 'k Value.repr -> ('k -> 'k -> nil, unit) relation
  | Filter : ('k Constant.hlist -> bool) * string -> ('k, unit) relation
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) * string
      -> ('k, unit) relation

type atom = Atom : ('k, 'v) relation * 'k Term.hlist -> atom

val print_atom : Format.formatter -> atom -> unit

val print_neg_atom : Format.formatter -> atom -> unit

val atom : ('k, 'v) relation -> 'k Term.hlist -> atom

val table : (_, 'k, 'v) Table.Id.t -> 'k Term.hlist -> atom

val unless : (_, 'k, 'v) Table.Id.t -> 'k Term.hlist -> atom

val distinct : 'k Value.repr -> 'k term -> 'k term -> atom

val filter :
  ?name:string -> ('k Constant.hlist -> bool) -> 'k Term.hlist -> atom

val callback_with_bindings :
  name:string ->
  (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) ->
  'k Term.hlist ->
  atom

type rule =
  { head : atom iarray;
    body : atom iarray
  }

val print_rule : Format.formatter -> rule -> unit

val rule : head:atom list -> body:atom list -> rule
