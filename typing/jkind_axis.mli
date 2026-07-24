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

    [t], the type of [Sort]-node slots (and the constant snapshots of sorts and
    layout variables: [Const.Base], [Const.Univar], [Const.Genvar]), describes a
    whole kind as a position in the fiber over its sort. [Exact a] is exactly
    the form obtained by applying the action [a] to the plain kind of the sort
    - deeply: an exactly-plain product sort has exactly-plain components, and an
      exactly whole-marked one also has exactly-plain components (the
      whole-product mark does not distribute); see [decomposed_component].
      [Join] denotes the *join* of the two exact forms - and, over a product
      sort, the top of the whole fiber of kinds at that sort: the marks are
      unknown deeply through the components. [Join] arises only for flexible
      bounds: a fresh sort variable's bound must admit every kind with whatever
      sort the variable resolves to, and it persists after the variable
      resolves, since resolving a sort determines none of the marks.

    Readings of a kind are separate types, so that a raw stored slot cannot be
    compared without going through a reading: [Mark.t] answers how the kind
    stands to whole-marking (the comparison operations live only there), and
    [Verdict.t] answers "is this kind addressable?". This is also not an
    [Axis.t]: it is not part of the mod- or with-bounds, but rather a component
    of layouts. *)
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
    | Exact of Action.t
    | Join

  (** An alias for [t], for referring to the slot type from within [Mark]. *)
  type slot = t

  (** Structural slot equality, for the constant snapshots whose carriers never
      resolve ([Const.Univar]/[Const.Genvar]). Everything else must compare
      *readings* ([Mark.equal] etc.), never raw slots. *)
  val equal : t -> t -> bool

  (** The slot a *flexible* sort-variable bound gets when an [any] bound's
      action is transferred onto it: applying no action leaves the whole fiber
      admitted ([Join]) - in contrast with a *rigid* bound (an lpoly layout
      variable, which stands for one specific unknown layout), which transfers a
      pending action exactly ([Exact]). *)
  val of_action_on_undetermined : Action.t -> t

  (** The action recorded in a [Sort] node's slot, forgetting the join. Used
      when flattening a product sort into a [Product] node, whose root carries
      only an action: the join, if any, moves onto the fabricated components
      instead ([decomposed_component]). *)
  val forget_join : t -> Action.t

  (** The slot fabricated for the components of a decomposed sort-backed product
      whose root slot is the argument: exact roots have exactly-plain
      components, whether the whole is plain or whole-marked (the whole-product
      mark does not distribute). Only the flexible [Join] leaves the components
      unconstrained. *)
  val decomposed_component : t -> t

  (** Only [Addressable] is ever printed in user-facing output. *)
  val to_string : t -> string

  val print : Format_doc.formatter -> t -> unit

  (** The result type of *mark* readings of a kind ([Jkind.Layout.mark]), which
      answer how the kind stands to whole-marking: [Marked] means the kind
      equals its whole-marked form (marked outright, committed, or over a sort
      where every form coincides), so marking it is a no-op; [Unmarked] means
      its root is exactly unmarked and the kind is not known addressable, so it
      is distinct from its whole-marked form and fails an [addressable]
      requirement; [Flexible] means the root mark is flexible (a [Join] slot,
      which may yet be committed). On a [Sort] node this is exactly which form
      over the sort the kind is (any slot collapses to [Marked] once the sort
      resolves intrinsically addressable); on an unmarked [Product] it is
      derived from the components ([combine_product]). A mark reading of
      [Unmarked] says nothing about whether the kind is addressable: the plain
      form of a rigid layout variable [x] reads [Unmarked] - it is distinct from
      [x addressable] *as polymorphic kinds* - while the addressability of [x]
      itself stays open (that question is [Verdict.t], whose [Undetermined] is
      epistemic where [Flexible] is constrainable).

      As an order on marks this is flat and partial: [Marked] and [Unmarked] are
      incomparable (the operator is a modifier, not a narrowing), and both are
      below [Flexible]. Accordingly, [meet] is partial. *)
  module Mark : sig
    type t =
      | Marked
      | Unmarked
      | Flexible

    (** The raw embedding: the mark a slot denotes verbatim. Readers apply the
        sort's collapse before falling back to this. *)
    val of_slot : slot -> t

    (** Store a computed mark back as a slot, for the meets' write-back. Sound
        only because meets return marks that describe the written node exactly
        (a meet of exact marks, or the flexible join). *)
    val to_slot : t -> slot

    val equal : t -> t -> bool

    val less_or_equal : t -> t -> Misc.Le_result.t

    val le : t -> t -> bool

    (** The meet of [Marked] and [Unmarked] is nothing; [Flexible] is the
        identity. *)
    val meet : t -> t -> t option

    (** The mark reading of an unmarked product from its components' readings:
        [Marked] when every component reads [Marked] (whole-marking the product
        is then a no-op), [Unmarked] when any component reads [Unmarked] (even
        if others are marked: the product is then distinct from its whole-marked
        form and not known addressable), and otherwise [Flexible]. This is not
        the [meet] of the components. *)
    val combine_product : t list -> t

    (** Whether kinds with these marks are all addressable (equivalently,
        [combine_product] of them is [Marked]): a whole-product mark over them
        would be a no-op, so e.g. printing one is uninformative. *)
    val all_marked : t list -> bool

    (** The mark reading of an action stored on a node whose carrier's intrinsic
        addressability is undetermined ([any] or an abstract kind): applying no
        action leaves the mark undetermined. *)
    val of_action_on_undetermined : Action.t -> t

    (** Only [Marked] is ever printed in user-facing output (as
        ["addressable"]). *)
    val to_string : t -> string
  end

  (** The result of an addressability *verdict* - "is this kind addressable?" -
      as opposed to a mark reading ([Mark.t]), which answers which form over its
      sort a kind is. The two differ exactly where honesty requires it: the
      plain form of an unresolved layout reads mark [Unmarked] but verdict
      [Undetermined]. *)
  module Verdict : sig
    type t =
      | Known_unaddressable
          (** Definitely not addressable: an unmarked (identity) form over a
              carrier resolved intrinsically unaddressable. *)
      | Known_addressable  (** Definitely addressable. *)
      | Undetermined
          (** Not determined: a flexible join, an unmarked form over an
              unresolved sort (rigid or flexible), [any], or an abstract kind.
          *)

    (** Whether kinds with these verdicts can possibly meet: [false] exactly
        when one is definitely addressable and the other definitely not.
        [Undetermined] is consistent with everything. *)
    val consistent : t -> t -> bool

    (** The verdict on a product of kinds from its components' verdicts:
        definitely not if any component is, otherwise undetermined if any
        component is, otherwise definitely addressable. *)
    val combine_product : t list -> t

    (** The verdict on a node whose carrier's intrinsic addressability is
        undetermined ([any] or an abstract kind): marked means definitely
        addressable; unmarked leaves the verdict undetermined. *)
    val of_action_on_undetermined : Action.t -> t
  end
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
