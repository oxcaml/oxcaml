(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Kinds and subkinds of Flambda types. *)

module Naked_number_kind : sig
  type t =
    | Naked_immediate
    | Naked_float32
    | Naked_float
    | Naked_int8
    | Naked_int16
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Naked_vec128
    | Naked_vec256
    | Naked_vec512

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

(** The kinds themselves. *)
type t = private
  | Value  (** OCaml values, either immediates or pointers. *)
  | Naked_number of Naked_number_kind.t
      (** The kind of unboxed numbers and untagged immediates. *)
  | Region
      (** Values which have been introduced by Flambda and are never accessible
          at the source language level (for example sets of closures). *)
  | Rec_info
      (** Recursion depths of identifiers. Like [Region], not accessible at the
          source level, but also not accessible at run time. *)

type kind = t

(** Constructors for the various kinds. *)
val value : t

val naked_number : Naked_number_kind.t -> t

val naked_immediate : t

val naked_float32 : t

val naked_float : t

val naked_int8 : t

val naked_int16 : t

val naked_int32 : t

val naked_int64 : t

val naked_nativeint : t

val naked_vec128 : t

val naked_vec256 : t

val naked_vec512 : t

val region : t

val rec_info : t

val is_value : t -> bool

val is_naked_float : t -> bool

include Container_types.S with type t := t

type flat_suffix_element = private
  | Naked_float
  | Naked_float32
  | Naked_int8
  | Naked_int16
  | Naked_int32
  | Naked_int64
  | Naked_nativeint
  | Naked_vec128
  | Naked_vec256
  | Naked_vec512

module Mixed_block_lambda_shape = Mixed_block_shape

module Mixed_block_shape : sig
  type t

  val from_mixed_block_shape : _ Mixed_block_lambda_shape.t -> t

  val field_kinds : t -> kind array

  val value_prefix_size : t -> int

  val flat_suffix : t -> flat_suffix_element array

  val size_in_words : t -> int

  val offset_in_words : t -> int -> int

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module Scannable_block_shape : sig
  type t =
    | Value_only
    | Mixed_record of Mixed_block_shape.t

  (** For now if two block shapes do not compare as equal they will be
      incompatible. If that changes, a [compatible] function will be
      introduced. *)
  val equal : t -> t -> bool

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit

  val element_kind : t -> int -> kind
end

module Block_shape : sig
  type t =
    | Scannable of Scannable_block_shape.t
    | Float_record

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit

  val element_kind : t -> int -> kind
end

module Standard_int : sig
  (** "Standard" because these correspond to the usual representations of tagged
      immediates, 32-bit, 64-bit and native integers as expected by the
      operations in [Flambda_primitive]. *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int8
    | Naked_int16
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end

module Boxable_number : sig
  (** These kinds are those of the numbers for which a tailored boxed
      representation exists. *)

  type t =
    | Naked_float32
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Naked_vec128
    | Naked_vec256
    | Naked_vec512

  val unboxed_kind : t -> kind

  val primitive_kind : t -> Primitive.boxed_integer

  val print_lowercase : Format.formatter -> t -> unit

  val print_lowercase_short : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end

module With_subkind : sig
  type full_kind

  module Nullable : sig
    type t =
      | Nullable
      | Non_nullable
  end

  (* Note: the current representation stores a non_null_value_subkind for every
     kind, even though it is only relevant for [Value] kinds. Other kinds should
     use the [Anything] constructor. *)
  module Non_null_value_subkind : sig
    type t =
      | Anything
      | Boxed_float32
      | Boxed_float
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Boxed_vec128
      | Boxed_vec256
      | Boxed_vec512
      | Tagged_immediate
      | Variant of
          { consts : Targetint_31_63.Set.t;
            non_consts : (Block_shape.t * full_kind list) Tag.Scannable.Map.t
          }
      | Float_block of { num_fields : int }
      | Float_array
      | Immediate_array
      | Value_array
      | Generic_array
      | Unboxed_float32_array
      | Unboxed_int32_array
      | Unboxed_int64_array
      | Unboxed_nativeint_array
      | Unboxed_vec128_array
      | Unboxed_vec256_array
      | Unboxed_vec512_array
      | Unboxed_product_array

    include Container_types.S with type t := t
  end

  type t = full_kind

  val create : kind -> Non_null_value_subkind.t -> Nullable.t -> t

  val anything : kind -> t

  val kind : t -> kind

  val non_null_value_subkind : t -> Non_null_value_subkind.t

  val nullable : t -> Nullable.t

  val has_useful_subkind_info : t -> bool

  (* Note: all constructors below assume non-nullability, except when noted *)

  (* [any_value] is nullable *)
  val any_value : t

  val naked_immediate : t

  val naked_float32 : t

  val naked_float : t

  val naked_int8 : t

  val naked_int16 : t

  val naked_int32 : t

  val naked_int64 : t

  val naked_nativeint : t

  val naked_vec128 : t

  val naked_vec256 : t

  val naked_vec512 : t

  val region : t

  val boxed_float : t

  val boxed_int32 : t

  val boxed_int64 : t

  val boxed_nativeint : t

  val boxed_vec128 : t

  val tagged_immediate : t

  val rec_info : t

  val float_array : t

  val immediate_array : t

  val value_array : t

  val generic_array : t

  val unboxed_vec128_array : t

  val unboxed_vec256_array : t

  val unboxed_vec512_array : t

  val unboxed_product_array : t

  val block : Tag.t -> t list -> t

  val float_block : num_fields:int -> t

  val of_naked_number_kind : Naked_number_kind.t -> t

  val naked_of_boxable_number : Boxable_number.t -> t

  val boxed_of_boxable_number : Boxable_number.t -> t

  (* Nullability is taken from the Lambda value kind *)
  val from_lambda_value_kind : Lambda.value_kind -> t

  (* Nullability is taken from the Lambda value kind *)
  val from_lambda_values_and_unboxed_numbers_only : Lambda.layout -> t

  val compatible : t -> when_used_at:t -> bool

  val erase_subkind : t -> t

  include Container_types.S with type t := t

  val equal_ignoring_subkind : t -> t -> bool

  val must_be_gc_scannable : t -> bool

  val may_be_gc_scannable : t -> bool
end

module Flat_suffix_element : sig
  type t = flat_suffix_element

  val naked_float : t

  val kind : t -> kind

  val from_singleton_mixed_block_element :
    _ Mixed_block_lambda_shape.Singleton_mixed_block_element.t -> t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val to_kind_with_subkind : t -> With_subkind.t
end

module Standard_int_or_float : sig
  (** The same as [Standard_int], but also permitting naked floats. *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_float32
    | Naked_float
    | Naked_int8
    | Naked_int16
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val of_standard_int : Standard_int.t -> t

  val to_kind : t -> kind

  val to_kind_with_subkind : t -> With_subkind.t

  val print_lowercase : Format.formatter -> t -> unit

  include Container_types.S with type t := t
end
