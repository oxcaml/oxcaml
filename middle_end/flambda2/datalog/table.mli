(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type _ result_repr

val unit_repr : unit result_repr

val lattice_repr :
  print:(Format.formatter -> 'a -> unit) ->
  meet:('a -> 'a -> 'a) ->
  diff_or_null:('a -> 'a -> 'a Or_null.t) ->
  'a result_repr

val result_repr_print : 'v result_repr -> Format.formatter -> 'v -> unit

val result_repr_union : 'v result_repr -> 'v -> 'v -> 'v

val result_repr_diff_or_null : 'v result_repr -> 'v -> 'v -> 'v Or_null.t

val result_repr_is_unit : 'v result_repr -> ('v, unit) Type.eq option

val union : ('t, 'k, 'v) Column.hlist -> 'v result_repr -> 't -> 't -> 't

module Id : sig
  type (!'t, !'k, !'v) t

  val print : Format.formatter -> ('t, 'k, 'v) t -> unit

  val hash : ('t, 'k, 'v) t -> int

  val uid : ('t, 'k, 'v) t -> int

  val name : ('t, 'k, 'v) t -> string

  (* Will raise [Misc.Fatal_error] if the two [Id.t]s do not have the same
     [uid]. *)
  val provably_equal_keys_exn :
    ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> ('k1, 'k2) Type.eq

  val provably_equal_exn :
    ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> ('t1, 't2) Type.eq

  (* Will raise [Misc.Fatal_error] if the two [Id.t]s do not have the same
     [uid]. *)
  val cast_exn : ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> 't1 -> 't2

  val equal : (_, _, _) t -> (_, _, _) t -> bool

  val compare : (_, _, _) t -> (_, _, _) t -> int

  val columns : ('t, 'k, 'v) t -> ('t, 'k, 'v) Column.hlist

  val result_repr : ('t, 'k, 'v) t -> 'v result_repr

  val has_provenance : ('t, 'k, 'v) t -> bool

  type ('k, 'v) poly = Id : ('t, 'k, 'v) t -> ('k, 'v) poly

  val create :
    provenance:bool ->
    name:string ->
    columns:('t, 'k, 'v) Column.hlist ->
    result_repr:'v result_repr ->
    ('t, 'k, 'v) t

  module Map : sig
    module type S = sig
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type t

      type 'a elt

      type binding = B : ('t, 'k, 'v) id * 't elt -> binding

      val empty : t

      val singleton : ('t, 'k, 'v) id -> 't elt -> t

      val add : ('t, 'k, 'v) id -> 't elt -> t -> t

      val remove : ('t, 'k, 'v) id -> t -> t

      val find : ('t, 'k, 'v) id -> t -> 't elt

      val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a

      val iter : (binding -> unit) -> t -> unit
    end

    module Make (T : sig
      type 'a t
    end) : S with type 'a elt = 'a T.t

    module Or_null_sender : S with type 'a elt = 'a Channel.or_null_sender

    module Or_null_receiver : S with type 'a elt = 'a Channel.or_null_receiver
  end

  module Tbl : sig
    module type S = sig
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type 'a elt

      type binding = B : ('a, _, _) id * 'a elt -> binding

      type t

      val create : int -> t

      val replace : t -> ('t, 'k, 'v) id -> 't elt -> unit

      val find : t -> ('t, 'k, 'v) id -> 't elt

      val iter : (binding -> unit) -> t -> unit
    end

    module Make (T : sig
      type 'a t
    end) : S with type 'a elt = 'a T.t
  end
end

module Map : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val is_empty : t -> bool

  val get_or_null : ('t, 'k, 'v) Id.t -> t -> 't Or_null.t

  val set_or_null : ('t, 'k, 'v) Id.t -> 't Or_null.t -> t -> t

  val set : ('t, 'k, 'v) Id.t -> 't -> t -> t

  val concat : earlier:t -> later:t -> t

  type binding = Binding : ('t, 'k, 'v) Id.t * 't -> binding

  val fold : f:(binding -> 'a -> 'a) -> t -> init:'a -> 'a

  val send : t -> Id.Map.Or_null_sender.t -> unit
end
