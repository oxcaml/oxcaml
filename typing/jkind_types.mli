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

(** You should use the types defined in [Jkind] (which redefines the types in
    this file) rather than using this file directly, unless you are in [Types]
    or [Primitive]. *)

(* This module defines types used in the module Jkind. This is to avoid a mutual
   dependencies between jkind.ml(i) and types.ml(i) and bewteen jkind.ml(i) and
   primitive.ml(i). Polymorphic versions of types are defined here, with type
   parameters that are meant to be filled by types defined in
   types.ml(i). jkind.ml(i) redefines the types from this file types.ml with the
   type variables instantiated. types.ml also redefines the types from this file
   with the type variables instantiated, but only for internal
   use. primitive.ml(i) uses the type [Jkind.Const.t], and types.ml(i) depends
   on primitive.ml(i), so [Jkind.Const.t] is defined here and primitive.ml(i)
   also uses this module.

   Dependency chain without Jkind_types:
         _____________________
         |         |         |
         |         |         V
   Primitive <-- Types <-- Jkind

   Dependency chain with Jkind_types:
        ______________________________________
        |                          |         |
        V                          |         |
   Jkind_types <-- Primitive <-- Types <-- Jkind

   All definitions here are commented in jkind.ml or jkind.mli. *)

module Sort : sig
  (* We need to expose these details for use in [Jkind] *)

  (* Comments in [Jkind_intf.ml] *)
  type base =
    | Void
    | Scannable
    | Untagged_immediate
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512

  val to_string_base : base -> string

  val equal_base : base -> base -> bool

  (** Whether types of this base sort store all of their information in the data
      portion of a block when boxed, so that interior pointers into the box can
      address them. *)
  val base_is_addressable : base -> bool

  type univar = { name : string option }

  type t =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar
    | Addressable of t
        (** [Addressable s] is the sort [s addressable]: a sort whose types
            store all of their information in the data portion of a block when
            boxed. Applying the operator to an already-addressable sort is the
            identity ([s addressable = s] whenever [s] is addressable), so the
            image of the operator is exactly the set of addressable sorts. Hence
            [Addressable (Var v)] with [v] unfilled represents "some addressable
            sort", and this constructor doubles as the constrained-addressable
            form used during inference (see [constrain_addressable]).

            Invariant: [Addressable s] is only built (via [addressable]) when
            [s] is not known to be addressable at construction time. The
            invariant can be broken by later instantiation of a variable inside
            [s], leaving a redundant wrapper — including a nested reading like
            [Addressable (Addressable _)] (equal to its payload) when the
            variable is filled with another wrapped sort. So matching on
            [Addressable] requires smart destruction, not just smart
            construction: readers ([get], [default_to_scannable_and_get], the
            predicates) re-normalize, and comparisons must too (see the [get]
            normalization in [equate_sort_addressable] — in particular, two
            wrapped sorts may not be compared by peeling one wrapper from each
            side without it). Nothing may match on [Addressable] and treat the
            wrapped sort as distinct from an addressable sort without first
            checking addressability. *)

  and var

  include
    Jkind_intf.Sort
      with type t := t
       and type var := var
       and type univar := univar
       and type base := base

  val set_change_log : (change -> unit) -> unit

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  val equate_tracking_mutation : t -> t -> equate_result

  (** Post-condition (which holds deeply within the sort): If the result is a
      [Var v], then [!v] is [None]. *)
  val get : t -> t

  (** Determines if the sort is [Scannable] or an unfilled sort variable *)
  val is_scannable_or_var : t -> bool

  (** Whether the sort is known to be addressable. [false] means "not known to
      be addressable": the sort may be definitely unaddressable (e.g. [bits8])
      or not yet determined (an unfilled variable, or a univar). *)
  val is_known_addressable : t -> bool

  (** The [addressable] operator. Absorbs (returning the argument unchanged)
      when the argument is already addressable. This is the only way to build
      the [Addressable] constructor. *)
  val addressable : t -> t

  (** Decompose a sort into a list (of the given length) of fresh sort
      variables, equating the input sort with the product of the output sorts.
  *)
  val decompose_into_product : t -> int -> t list option

  (** Like [decompose_into_product], but equates the input sort with the product
      of the output sorts made addressable, i.e. decomposes it as an addressable
      product. *)
  val decompose_into_addressable_product : t -> int -> t list option

  (** Require the sort to be an addressable sort (equating it with a fresh
      variable made addressable), returning [false] if it cannot be one. *)
  val constrain_addressable : t -> bool

  module Flat : sig
    type t =
      | Var of Var.id
      | Genvar of var
      | Univar of univar
      | Base of base
  end
end

module Kind_operator : sig
  (** An operator application pending on an abstract kind constructor
      ([Types.jkind_base.Kconstr]). Since [jkind_base] is not recursive, the
      [addressable] operator applied to a [Kconstr] is recorded here and applied
      to the expansion whenever the constructor is expanded. [Id] is "no
      operator". *)
  type t =
    | Id
    | Addressable

  val equal : t -> t -> bool
end

module Scannable_axes : sig
  type t =
    { nullability : Jkind_axis.Nullability.t;
      separability : Jkind_axis.Separability.t
    }

  val max : t

  val value_axes : t

  val equal : t -> t -> bool

  val less_or_equal : t -> t -> Misc.Le_result.t

  val meet : t -> t -> t
end

module Layout : sig
  (** Note that:

      1. Products have two possible encodings: as [Product ...] or as
      [Sort (Product ...]. This duplication is hard to eliminate because of the
      possibility that a sort variable may be instantiated by a product sort.

      2. Scannable axes are meaningful only when the layout might be scannable
      ([any], [scannable], a sort variable, or an abstract kind). On other
      layouts they are ignored, so e.g. [float64 non_pointer] is equivalent to
      [float64]. See [Layout.Const.get_root_scannable_axes]. *)
  type 'sort t =
    | Sort of 'sort * Scannable_axes.t
    | Product of 'sort t list
    | Any of Scannable_axes.t
    | Addressable of 'sort t
        (** The [addressable] operator, applied to a layout. Its meaning depends
            on what it is applied to:

            - On [Any sa] it is a *bound*: [Addressable (Any sa)] (written
              [any addressable]) is the top of all addressable layouts. Every
              addressable layout is below it, and it is strictly below [Any sa].

            - On anything else it is a *claim*: a new, made-addressable layout
              that is neither above nor below its argument — unless the argument
              is already addressable, in which case the operator absorbs
              ([l addressable = l]) and this constructor must not appear.

            Invariant (for [Sort.t t] only): the argument is [Any _] or
            [Product _]. It is never [Sort _], because addressability of a sort
            slot is pushed into the sort itself (see [Sort.t]'s [Addressable]),
            so that the constraint travels with the sort during unification. The
            flattened [Sort.Flat.t t] view produced by [Jkind.Layout.get]
            relaxes this: there, [Addressable] may wrap [Sort] nodes.

            As with sorts, instantiating a sort variable inside a wrapped
            [Product] payload can make the payload addressable and hence the
            wrapper redundant, so matching on [Addressable] requires smart
            destruction: comparisons apply [strip_redundant_addressable] before
            matching. (Unlike sorts, nested wrappers cannot arise here: layout
            nodes are immutable and only built by [addressable].) *)

  module Const : sig
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base * Scannable_axes.t
      | Product of t list
      | Univar of Sort.univar
      | Genvar of Sort.var
          (** A layout variable bound by a surrounding [val_lpoly]. It's a
              "fake" constant that will be instantiated to real layout constant
              by slambda. The [var] is used only for physical identity; its
              contents are not consumed and its level must be
              [Ident.highest_scope]. *)
      | Addressable of t
          (** See the comment on [Layout.t]'s [Addressable] constructor. Here
              there is no [Sort] node to push into, so [Addressable] may
              additionally wrap [Base], [Univar], and [Genvar]. *)

    module Static : sig
      val of_base : Sort.base -> Scannable_axes.t -> t
    end

    val equal : t -> t -> bool

    val max : t

    val get_sort : t -> Sort.Const.t option

    val is_scannable_or_any : t -> bool

    (** Whether the layout is known to be addressable. [false] means "not known
        to be addressable"; in particular [Any _] is [false], as [any] has
        unaddressable subkinds. *)
    val is_known_addressable : t -> bool

    (** The [addressable] operator. Absorbs (returning the argument unchanged)
        when the argument is already addressable. This is the only way to build
        the [Addressable] constructor. *)
    val addressable : t -> t

    (** Returns [None] if the root of [t] has no meaningful scannable axes (e.g.
        [Base Float64], [Product], [Univar], [Genvar]). *)
    val get_root_scannable_axes : t -> Scannable_axes.t option

    (** Updates the scannable axes at the root of [t] (changes nothing when
        [get_root_scannable_axes] would return [None]). *)
    val set_root_scannable_axes : t -> Scannable_axes.t -> t

    (** Meets [sa] into [t]'s root scannable axes (if [t] has meaningful ones;
        otherwise returns [t] unchanged). *)
    val meet_root_scannable_axes : t -> Scannable_axes.t -> t
  end

  val of_const : Const.t -> Sort.t t

  val of_new_sort_var : level:int -> Scannable_axes.t -> Sort.t t * Sort.t

  val get_const : Sort.t t -> Const.t option

  val get_flat_const : Sort.Flat.t t -> Const.t option

  val product : 'a t list -> 'a t

  (** Whether the layout is known to be addressable. See
      [Const.is_known_addressable]. *)
  val is_known_addressable : Sort.t t -> bool

  (** The [addressable] operator on layouts. Absorbs when the argument is
      already addressable, and pushes into the sort of a [Sort] node (see the
      invariant on [Addressable]). This is the only way to build the
      [Addressable] constructor. *)
  val addressable : Sort.t t -> Sort.t t

  (** The smart destructor paired with [addressable]: removes wrappers made
      redundant by sort-variable instantiation (a wrapper whose payload has
      become addressable equals its payload). Comparisons must apply this before
      matching on [Addressable]. *)
  val strip_redundant_addressable : Sort.t t -> Sort.t t
end
