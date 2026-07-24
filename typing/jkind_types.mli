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

  type univar = { name : string option }

  type t =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar

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

  (** Decompose a sort into a list (of the given length) of fresh sort
      variables, equating the input sort with the product of the output sorts.
  *)
  val decompose_into_product : t -> int -> t list option

  module Flat : sig
    type t =
      | Var of Var.id
      | Genvar of var
      | Univar of univar
      | Base of base
  end
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

(** See [Jkind_axis.Addressability] for an overview of actions ([Action.t], on
    [Any]/[Product]/[Kconstr] nodes) vs the full [t] with [Exact] forms and the
    [Join] (on [Sort] nodes, also the type of normalized-mark readings) vs
    verdicts ([Verdict.t], the type of "is this kind addressable?" answers). *)
module Addressability : sig
  module Action : sig
    type t = Jkind_axis.Addressability.Action.t =
      | Id
      | Addressable

    val equal : t -> t -> bool

    val compose : t -> t -> t

    val to_string : t -> string

    val print : Format_doc.formatter -> t -> unit
  end

  type t = Jkind_axis.Addressability.t =
    | Exact of Action.t
    | Join

  val equal : t -> t -> bool

  val less_or_equal : t -> t -> Misc.Le_result.t

  val le : t -> t -> bool

  val meet : t -> t -> t option

  val combine_product : t list -> t

  val of_action_on_undetermined : Action.t -> t

  val forget_join : t -> Action.t

  val decomposed_component : t -> t

  val to_string : t -> string

  val print : Format_doc.formatter -> t -> unit

  module Verdict : sig
    type t = Jkind_axis.Addressability.Verdict.t =
      | Unaddressable
      | Addressable
      | Undetermined

    val consistent : t -> t -> bool

    val combine_product : t list -> t

    val of_action_on_undetermined : Action.t -> t
  end

  (** Whether every kind at this base sort is addressable, whatever its marks:
      the plain kind is intrinsically addressable, so marking is a no-op and all
      forms coincide. [Scannable], [Word], [Bits64], and the vector bases are
      boxed as blocks with all data in the body; the others are boxed as tagged
      immediates or float blocks. *)
  val base_is_always_addressable : Sort.base -> bool

  (** The intrinsic addressability of a sort, as a verdict about its *plain*
      kind, insofar as it is determined: the addressability of an unfilled sort
      variable is not yet known ([Undetermined]). This is a mark-free question,
      so it can be definite ([Unaddressable]) even while the marks over the sort
      are undetermined. *)
  val of_sort : Sort.t -> Verdict.t

  (** Whether every kind with this sort is addressable, whatever its marks: the
      plain kind is intrinsically addressable, so the plain, marked, and join
      forms all coincide, and readers may collapse unmarked and join slots to
      [Addressable]. *)
  val sort_is_always_addressable : Sort.t -> bool
end

module Layout : sig
  (** Note that:

      1. Products have two possible encodings: as [Product ...] or as
      [Sort (Product ...]. This duplication is hard to eliminate because of the
      possibility that a sort variable may be instantiated by a product sort.

      2. Scannable axes are meaningful only when the layout might be scannable
      ([any], [scannable], a sort variable, or an abstract kind). On other
      layouts they are ignored, so e.g. [float64 non_pointer] is equivalent to
      [float64]. See [Layout.Const.get_root_scannable_axes].

      3. Each node also carries an addressability slot, tracking applications of
      the [addressable] kind operator. [Product] and [Any] nodes carry a
      two-state action: marked ([Addressable]) or unmarked ([Id]; an unmarked
      product derives its addressability from its components at read time, and
      an unmarked [any] is the top kind). [Sort] nodes (and the [Const.Base],
      [Const.Univar], and [Const.Genvar] snapshots) carry a full
      [Addressability.t]: [Exact a] for exactly the form obtained by applying
      [a] to the plain kind of the sort (normalized to [Exact Addressable] on
      intrinsically addressable bases), and [Join] - the join of the two exact
      forms - for flexible bounds (fresh variables, and the components of
      decomposed products whose own root is the join; the analog of
      [Scannable_axes.max] there). See the comment on the implementation's
      [Layout.t] for the full invariants. *)
  type 'sort t =
    | Sort of 'sort * Scannable_axes.t * Addressability.t
    | Product of 'sort t list * Addressability.Action.t
    | Any of Scannable_axes.t * Addressability.Action.t

  module Const : sig
    type t =
      | Any of Scannable_axes.t * Addressability.Action.t
      | Base of Sort.base * Scannable_axes.t * Addressability.t
      | Product of t list * Addressability.Action.t
      | Univar of Sort.univar * Scannable_axes.t * Addressability.t
      | Genvar of Sort.var * Scannable_axes.t * Addressability.t
          (** A layout variable bound by a surrounding [val_lpoly]. It's a
              "fake" constant that will be instantiated to real layout constant
              by slambda. The [var] is used only for physical identity; its
              contents are not consumed and its level must be
              [Ident.highest_scope]. [Univar] and [Genvar] carry their scannable
              axes and addressability so a variable's bound survives a cmi round
              trip losslessly. *)

    module Static : sig
      (** Normalizes the addressability slot to [Addressable] on intrinsically
          addressable bases (e.g. [bits64 addressable] IS [bits64]). *)
      val of_base : Sort.base -> Scannable_axes.t -> Addressability.t -> t
    end

    val equal : t -> t -> bool

    val max : t

    val get_sort : t -> Sort.Const.t option

    val is_scannable_or_any : t -> bool

    (** The normalized mark of a constant layout: which of the forms over its
        sort the kind is. A [Join] slot on a base collapses only when the base
        is intrinsically addressable, where its branches coincide, and is
        otherwise preserved - the join is information (a flexible [bits8] bound
        still admits [bits8 addressable]) and must survive [equal]; only
        printing elides it. An unmarked product derives its mark from its
        components; [Univar]/[Genvar] layouts read their stored slot ([Exact Id]
        on a rigid variable is exactly the plain form, with no claim about its
        addressability). *)
    val normalized_mark : t -> Addressability.t

    (** Returns [None] if the root of [t] has no meaningful scannable axes (e.g.
        [Base Float64], [Product], [Univar], [Genvar]). *)
    val get_root_scannable_axes : t -> Scannable_axes.t option

    (** Updates the scannable axes at the root of [t] (changes nothing when
        [get_root_scannable_axes] would return [None]). *)
    val set_root_scannable_axes : t -> Scannable_axes.t -> t

    (** Meets [sa] into [t]'s root scannable axes (if [t] has meaningful ones;
        otherwise returns [t] unchanged). *)
    val meet_root_scannable_axes : t -> Scannable_axes.t -> t

    (** Apply the [addressable] kind operator: an override of the root slot to
        [Addressable], not a meet. This does nothing to an already-addressable
        kind. *)
    val set_root_addressable : t -> t
  end

  val of_const : Const.t -> Sort.t t

  val of_new_sort_var : level:int -> Scannable_axes.t -> Sort.t t * Sort.t

  val get_const : Sort.t t -> Const.t option

  val get_flat_const : Sort.Flat.t t -> Const.t option

  val product : 'a t list -> 'a t
end
