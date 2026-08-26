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

(** See Note [Kind properties] in [jkind_intf.ml]. *)
module type Property = sig
  type t

  val id : t

  val is_id : t -> bool

  val equal : t -> t -> bool

  (** [compose t1 t2] denotes [t1 ° t2] (equivalently [t2 ° t1]). *)
  val compose : t -> t -> t

  (** The order for which [compose] is the meet and [id] is the top. *)
  val less_or_equal : t -> t -> Misc.Le_result.t

  (** [residual ~have t] is the part of [t] that [have] does not already
      enforce. [is_id (residual ~have t)] exactly when [have x] is a fixed point
      of [t] for every [x]. *)
  val residual : have:t -> t -> t

  (** The kind modifiers spelling out [t], in the order they are printed. *)
  val to_string_list : t -> string list
end

module Addressability : sig
  type t =
    | Id
    | Addressable

  include Property with type t := t
end

module Scannable_axes : sig
  type t =
    { nullability : Jkind_axis.Nullability.t;
      separability : Jkind_axis.Separability.t
    }

  include Property with type t := t

  val max : t

  val value_axes : t

  val le : t -> t -> bool

  val meet : t -> t -> t

  (** [to_string_list_diff ~base t] lists the modifiers needed to get from
      [base] to [t]; [None] if [t] is not below [base] on every axis. *)
  val to_string_list_diff : base:t -> t -> string list option
end

(** The property-enforcing operators a kind may carry, bundled together. The
    components commute, so everything is computed componentwise. *)
module Prop : sig
  type t =
    { addressability : Addressability.t;
      scannable_axes : Scannable_axes.t
    }

  include Property with type t := t

  val create :
    addressability:Addressability.t -> scannable_axes:Scannable_axes.t -> t

  val addressable : t

  val of_scannable_axes : Scannable_axes.t -> t

  val is_addressable : t -> bool

  (** The residual of [t] for a term that can never be [Scannable]: such a term
      is automatically a fixed point of the scannable-axes component. *)
  val on_unscannable : t -> t
end

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
    | Mask

  val to_string_base : base -> string

  val equal_base : base -> base -> bool

  val base_is_addressable : base -> bool

  val base_is_scannable : base -> bool

  type univar = { name : string option }

  (** A sort [{ prop; data }] denotes the operator [prop] applied to [data]. See
      Note [Kind properties] in [jkind_intf.ml]. *)
  type data =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar

  and t =
    { prop : Prop.t;
      data : data
    }

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

  type constrain_result =
    | Constrained_mutated
    | Constrained_no_mutation
    | Not_constrained

  (** [constrain_fixpoint ~prop t] establishes [t = prop t], mutating sort
      variables when that is allowed and necessary. *)
  val constrain_fixpoint :
    allow_mutation:bool -> prop:Prop.t -> t -> constrain_result

  val is_surely_fixpoint : prop:Prop.t -> t -> bool

  val is_surely_addressable : t -> bool

  (** Applies an operator to a sort; this is just composition. *)
  val apply_prop : Prop.t -> t -> t

  val of_data : data -> t

  val of_univar : univar -> t

  (** Splits the sort into the operator applied at its head — following through
      filled variables, whose contents may apply further operators — and the
      operator-free remainder. *)
  val split_head_prop : t -> Prop.t * t

  val strip_head_prop : t -> t

  (** The operator applied at the head of the sort. *)
  val head_prop : t -> Prop.t

  (** The part of the head operator that is not already implied by the sort's
      structure: the modifiers that need printing. *)
  val visible_prop : t -> Prop.t

  (** Post-condition (which holds deeply within the sort): If the result is a
      [Var v], then [!v] is [None]. *)
  val get : t -> t

  (** Determines if the sort is [Scannable] or an unfilled sort variable,
      possibly under [Addressable] wrappers *)
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

module Layout : sig
  (** Note that:

      1. Products have two possible encodings: as [Product ...] or as
      [Sort (Product ...]. This duplication is hard to eliminate because of the
      possibility that a sort variable may be instantiated by a product sort.

      2. Scannable axes are meaningful only when the layout might be scannable
      ([any], [scannable], a sort variable, or an abstract kind). On other
      layouts they are ignored, so e.g. [float64 non_pointer] is equivalent to
      [float64]. See [Layout.Const.get_root_scannable_axes].

      3. Like products, the operators in [prop] have two possible encodings: at
      the layout level or within a sort. Operators on [Any] can only be encoded
      at the layout level. *)
  type 'sort data =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  and 'sort t =
    { prop : Prop.t;
      data : 'sort data
    }

  val of_data : 'sort data -> 'sort t

  val of_sort : 'sort -> 'sort t

  val any : Prop.t -> 'sort t

  module Const : sig
    type t = private
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
          (** See Note [Kind properties].

              Invariant: this constructor is never redundantly applied. I.e.,
              given [Addressable t], [not (is_surely_addressable t)]. *)

    val any : Scannable_axes.t -> t

    val product : t list -> t

    val univar : Sort.univar -> t

    val genvar : Sort.var -> t

    module Static : sig
      val of_base : Sort.base -> Scannable_axes.t -> t
    end

    val equal : t -> t -> bool

    val max : t

    val get_sort : t -> Sort.Const.t option

    val is_scannable_or_any : t -> bool

    val is_surely_addressable : t -> bool

    val addressable : t -> t

    val apply_addressability : t -> Addressability.t -> t

    val apply_prop : t -> Prop.t -> t

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

  val of_new_sort_var : level:int -> Prop.t -> Sort.t t * Sort.t

  val get_const : Sort.t t -> Const.t option

  val get_flat_const : Sort.Flat.t t -> Const.t option

  val product : 'a t list -> 'a t

  val apply_prop : Prop.t -> 'a t -> 'a t
end
