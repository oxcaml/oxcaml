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
  type t =
    | External
    | External64
    | Internal

  include Axis_ops with type t := t

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

(** The addressability of a kind: whether its types store all of their
    information in the data portion of a block when boxed, making it possible to
    take interior pointers to them.

    Unlike the axes above, this is not a total order: [Addressable] and
    [Unaddressable] are incomparable (applying the [addressable] kind operator
    is a modifier, not a narrowing), and both are below [Maybe_addressable],
    which describes an unconstrained or not-yet-known addressability.
    Accordingly, [meet] is partial. This is also not an [Axis.t]: it is not part
    of the mod- or with-bounds, but rather a component of layouts. *)
module Addressability : sig
  type t =
    | Addressable
    | Unaddressable
    | Maybe_addressable

  val max : t

  val equal : t -> t -> bool

  val less_or_equal : t -> t -> Misc.Le_result.t

  val le : t -> t -> bool

  (** [meet Addressable Unaddressable] is [None]; [Maybe_addressable] is the
      identity. *)
  val meet : t -> t -> t option

  (** The addressability of a product of kinds with the given addressabilities:
      [Addressable] iff all components are addressable, [Unaddressable] if any
      component is unaddressable, and otherwise [Maybe_addressable]. This is not
      the [meet] of the components. *)
  val combine_product : t list -> t

  (** Only [Addressable] is ever printed in user-facing output. *)
  val to_string : t -> string

  val print : Format_doc.formatter -> t -> unit
end

module Axis : sig
  module Nonmodal : sig
    type 'a t = Externality : Externality.t t
  end

  (** Represents an axis of a jkind *)
  type 'a t =
    | Modal : 'a Mode.Crossing.Axis.t -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed [@@unboxed]

  val all : packed list

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

  (** A set of all modal axes *)
  val all_modal_axes : t

  (** A set of all nonmodal axes *)
  val all_nonmodal_axes : t

  val print : Format.formatter -> t -> unit
end
