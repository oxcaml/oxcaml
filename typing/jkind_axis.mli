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

(** Tracking of the [addressable] kind operator: a kind is addressable when its
    types store all of their information in the data portion of a block when
    boxed, making it possible to take interior pointers to them.

    [Action.t] records what was applied to a kind node: the [addressable]
    operator ([Addressable]) or nothing ([Id]). [Any], [Product], and [Kconstr]
    nodes store exactly an action.

    [t] adds a third value, [Id_or_addressable], denoting the *join* of a layout
    [L] and [L addressable] - and, over a product sort, the top of the whole
    fiber of kinds at that sort: the marks are unknown deeply through the
    components (one root slot suffices because decomposition regenerates the
    join on each fabricated component; see [decomposed_component]). It is stored
    only on [Sort] nodes and the constant snapshots of sorts and layout
    variables ([Const.Base], [Const.Univar], [Const.Genvar]), where it arises
    for flexible bounds: a fresh sort variable's bound must admit every kind
    with whatever sort the variable resolves to, and it persists after the
    variable resolves, since resolving a sort determines none of the marks. [t]
    is also the result type of addressability *readings* of a kind: there
    [Addressable] means definitely addressable, [Id] definitely not (the
    identity action on an intrinsically unaddressable carrier), and
    [Id_or_addressable] not (yet) determined.

    As an order on readings this is flat and partial: [Addressable] and [Id] are
    incomparable (the operator is a modifier, not a narrowing), and both are
    below [Id_or_addressable]. Accordingly, [meet] is partial. This is also not
    an [Axis.t]: it is not part of the mod- or with-bounds, but rather a
    component of layouts. *)
module Addressability : sig
  module Action : sig
    type t =
      | Id
      | Addressable

    val equal : t -> t -> bool

    (** [compose a a'] is the action [a] applied after [a']: [Addressable] if
        either is. Actions form a monoid with [Id] the identity; the operator is
        idempotent, so composition is commutative and coincides with the join.
        Used when expanding an abstract kind composes its pending action with
        its manifest's, and when an intersection of products is marked iff
        either input is. *)
    val compose : t -> t -> t

    (** Only [Addressable] is ever printed in user-facing output. *)
    val to_string : t -> string

    val print : Format_doc.formatter -> t -> unit
  end

  type t =
    | Id
    | Addressable
    | Id_or_addressable

  val equal : t -> t -> bool

  val less_or_equal : t -> t -> Misc.Le_result.t

  val le : t -> t -> bool

  (** [meet Addressable Id] is [None]; [Id_or_addressable] is the identity. *)
  val meet : t -> t -> t option

  (** The reading of a product of kinds from its components' readings: [Id] (not
      addressable) if any component is, otherwise [Id_or_addressable] if any
      component is undetermined, otherwise [Addressable]. This is not the [meet]
      of the components. *)
  val combine_product : t list -> t

  (** The exact embedding: the slot a *rigid* sort-variable bound (an lpoly
      layout variable, which stands for one specific unknown layout) gets when a
      pending action is transferred onto it. *)
  val of_action : Action.t -> t

  (** The reading of an action stored on a node whose carrier's intrinsic
      addressability is undetermined ([any], an abstract kind, or an unfilled
      sort variable): applying no action leaves the addressability undetermined.
      Also the slot a *flexible* sort-variable bound gets when an [any] bound's
      action is transferred onto it. *)
  val of_action_on_undetermined : Action.t -> t

  (** The action recorded in a [Sort] node's slot, forgetting the join. Used
      when flattening a product sort into a [Product] node, whose root carries
      only an action: the join, if any, moves onto the fabricated components
      instead ([decomposed_component]). *)
  val forget_join : t -> Action.t

  (** The slot fabricated for the components of a decomposed sort-backed product
      whose root slot is the argument. The exact roots have exact components:
      [Id] is exactly the plain product and [Addressable] exactly the
      whole-marked product, whose components are exactly plain in both cases
      (the whole-product mark does not distribute). Only the flexible join
      leaves the components unconstrained. *)
  val decomposed_component : t -> t

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
