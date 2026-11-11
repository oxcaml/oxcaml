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

    module Map : Container_types.Map with type key = t and module Set = Set

    module Tbl : Hashtbl.S with type key = t
  end

  val uid : 'a t -> Id.t

  val provably_equal : 'a t -> 'b t -> ('a, 'b) Type.eq option

  (* Raises [Misc.Fatal_error] if the two variables are not equal.

     Variables with the same [uid] are guaranteed to be equal. *)
  val must_be_equal : 'a t -> 'b t -> ('a, 'b) Type.eq

  module Map : sig
    module type S = sig
      type t

      type 'a elt

      type binding = B : 'a variable * 'a elt -> binding

      val empty : t

      val singleton : 'a variable -> 'a elt -> t

      val add : 'a variable -> 'a elt -> t -> t

      val remove : 'a variable -> t -> t

      val find : 'a variable -> t -> 'a elt

      val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
    end

    module Make (T : sig
      type 'a t
    end) : S with type 'a elt = 'a T.t
  end
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

  val print_hlist :
    pp_sep:(Format.formatter -> unit -> unit) ->
    Format.formatter ->
    _ hlist ->
    unit

  val print_columns :
    pp_sep:(Format.formatter -> unit -> unit) ->
    (_, 'a, _) Column.hlist ->
    Format.formatter ->
    'a hlist ->
    unit
end

type ('k, 'v) relation =
  | Table : (_, 'k, 'v) Table.Id.t -> ('k, 'v) relation
  | Unless : (_, 'k, 'v) Table.Id.t -> ('k, unit) relation
  | Distinct : (_, 'k, _) Column.id -> ('k -> 'k -> nil, unit) relation
  | Filter : ('k Constant.hlist -> bool) * string -> ('k, unit) relation
  | If_let : ('k Constant.hlist -> 'v Or_null.t) with_name -> ('k, 'v) relation
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) * string
      -> ('k, unit) relation

type atom =
  | Atom : ('k, 'v) relation * 'k Term.hlist * 'v variable option -> atom

val print_atom : Format.formatter -> atom -> unit

val print_neg_atom : Format.formatter -> atom -> unit

val atom : ('k, 'v) relation -> 'k Term.hlist -> atom

val table : (_, 'k, 'v) Table.Id.t -> 'k Term.hlist -> atom

val unless : (_, 'k, 'v) Table.Id.t -> 'k Term.hlist -> atom

val distinct : (_, 'k, _) Column.id -> 'k term -> 'k term -> atom

val filter :
  ?name:string -> ('k Constant.hlist -> bool) -> 'k Term.hlist -> atom

val if_let :
  ?name:string ->
  'v variable ->
  ('k Constant.hlist -> 'v Or_null.t) ->
  'k Term.hlist ->
  atom

val callback_with_bindings :
  name:string ->
  (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) ->
  'k Term.hlist ->
  atom

type rule =
  { variables : Variable.t_ list;
    head : atom iarray;
    body : atom iarray
  }

val print_rule : Format.formatter -> rule -> unit

val rule :
  variables:Variable.t_ list -> head:atom list -> body:atom list -> rule
