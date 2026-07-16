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
        (** Products and their components are implicitly addressable. The
            component nodes need not themselves contain [Addressable]. *)
    | Addressable of t
        (** An explicit addressability refinement. Smart constructors and
            observers remove this node around intrinsically-addressable sorts,
            products, and other [Addressable] nodes. *)
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
      | Addressable of t
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
        (** Addressability for a layout that cannot carry it in its sort. In
            canonical values this is [Addressable (Any _)]. *)

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
          (** Explicit addressability on a layout that is not intrinsically
              addressable. Unlike the live layout above, this constant form also
              uses the constructor for representable atomic layouts, because
              [Sort.Const.t] deliberately erases addressability. *)

    module Static : sig
      val of_base : Sort.base -> Scannable_axes.t -> t
    end

    val equal : t -> t -> bool

    val max : t

    (** Extracts the calling-convention sort, erasing addressability. Returns
        [None] for [Any], including [Any addressable]. *)
    val get_sort : t -> Sort.Const.t option

    val is_scannable_or_any : t -> bool

    (** Returns [None] if the root of [t] has no meaningful scannable axes (e.g.
        [Base Float64], [Product], [Univar], [Genvar]). *)
    val get_root_scannable_axes : t -> Scannable_axes.t option

    (** Updates the scannable axes at the root of [t] (changes nothing when
        [get_root_scannable_axes] would return [None]). *)
    val set_root_scannable_axes : t -> Scannable_axes.t -> t

    (** Meets [sa] into [t]'s root scannable axes (if [t] has meaningful ones;
        otherwise returns [t] unchanged). *)
    val meet_root_scannable_axes : t -> Scannable_axes.t -> t

    val addressable : t -> t

    val get_addressable : t -> t option
  end

  val of_const : Const.t -> Sort.t t

  val of_new_sort_var : level:int -> Scannable_axes.t -> Sort.t t * Sort.t

  val get_const : Sort.t t -> Const.t option

  val get_flat_const : Sort.Flat.t t -> Const.t option

  val product : 'a t list -> 'a t

  val addressable : Sort.t t -> Sort.t t

  val get_addressable : Sort.t t -> Sort.t t option
end
