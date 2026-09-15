(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Re-export *)
module type Axis_ops = sig
  include Mode_intf.Lattice

  val to_string : t -> string

  val less_or_equal : t -> t -> Misc.Le_result.t

  val equal : t -> t -> bool
end

(** The jkind axis of Externality *)
module Externality : sig
  type t = Mode.Externality.Const.t =
    | External
    | External64
    | Internal

  include Mode_intf.Lattice with type t := t

  val upper_bound_if_is_always_gc_ignorable : unit -> t
end

(** The jkind axis of nullability *)
module Nullability : sig
  type t =
    | Non_null
    | Maybe_null

  include Axis_ops with type t := t
end

module Separability : sig
  type t =
    | Non_pointer
    | Non_pointer64
    | Non_float
    | Separable
    | Maybe_separable

  include Axis_ops with type t := t

  val upper_bound_if_is_always_gc_ignorable : unit -> t
end

module Axis : sig
  type 'a t = 'a Mode.Crossing.Axis.t =
    | Monadic :
        'a Mode.Value.Monadic.Axis.t
        -> 'a Mode.Crossing.Monadic.Atom.t t
    | Comonadic :
        'a Mode.Value.Comonadic.Axis.t
        -> 'a Mode.Crossing.Comonadic.Atom.t t

  type packed = Pack : 'a t -> packed [@@unboxed]

  val all : packed list

  val equal : packed -> packed -> bool

  val name : _ t -> string
end

module Per_axis :
  Solver_intf.Lattices with type 'a elt := 'a and type 'a obj := 'a Axis.t

module Axis_set : sig
  (** A set of [Axis.t], represented as a bitfield for efficiency. *)
  type t [@@immediate]

  val empty : t

  val singleton : _ Axis.t -> t

  val is_empty : t -> bool

  val add : t -> _ Axis.t -> t

  val remove : t -> _ Axis.t -> t

  val mem : t -> _ Axis.t -> bool

  val equal : t -> t -> bool

  val union : t -> t -> t

  val intersection : t -> t -> t

  val diff : t -> t -> t

  val is_subset : t -> t -> bool

  val complement : t -> t

  val to_seq : t -> Axis.packed Seq.t

  val to_list : t -> Axis.packed list

  (** Create a [t], specify for each axis whether it should be included *)
  val create : f:(axis:Axis.packed -> bool) -> t

  (** A set of all axes *)
  val all : t

  val print : Format.formatter -> t -> unit
end
