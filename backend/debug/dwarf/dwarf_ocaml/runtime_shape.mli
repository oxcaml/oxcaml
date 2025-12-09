(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module Sort = Jkind_types.Sort
module Layout = Sort.Const

type 'a or_void =
  | Other of 'a
  | Void

val or_void_to_string : ('a -> string) -> 'a or_void -> string

val erase_void : 'a or_void list -> 'a list

val print_or_void :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a or_void -> unit

module Runtime_layout : sig
  type t =
    | Value
    | Float64
    | Float32
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512
    | Word
    | Untagged_immediate

  val size_in_memory : t -> int

  val size : t -> int

  val of_base_layout : Sort.base -> t or_void

  val to_string : t -> string

  val print : Format.formatter -> t -> unit

  val to_base_layout : t -> Sort.base

  val equal : t -> t -> bool

  val hash : t -> int
end

(** Runtime shape, simplified shapes that can actually occur at runtime (with a
    runtime layout). *)
type t = private
  { desc : desc;
    runtime_layout : Runtime_layout.t;
    hash : int
  }

and desc = private
  | Unknown of Runtime_layout.t
  | Predef of predef
  | Tuple of
      { args : t list;
        kind : tuple_kind
      }
  | Variant of
      { constructors : constructors;
        kind : variant_kind
      }
  | Record of
      { fields : string mixed_block_field list;
        kind : record_kind
      }
  | Func
  | Mu of t
  | Rec_var of Shape.DeBruijn_index.t * Runtime_layout.t
(* CR sspies: Use regular identifiers beforehand and only switch to DeBruijn at
   this level. *)

and tuple_kind = private
  | Tuple_boxed
      (** Same treatment as a mixed block; can contain anything in the near future. *)

and variant_kind = private
  | Variant_boxed
  | Variant_attribute_unboxed of Runtime_layout.t
  | Variant_polymorphic

and record_kind = private
  | Record_attribute_unboxed of Runtime_layout.t
  | Record_mixed

and 'label mixed_block_field = private
  { field_type : t;
    label : 'label
  }

and constructor = private
  | Constructor_with_tuple_arg of
      { name : string;
        args : unit mixed_block_field list
      }
  | Constructor_with_record_arg of
      { name : string;
        args : string mixed_block_field list
      }

and constructors = constructor list

and predef =
  | Array of array_kind
  | Bytes
  | Char
  | Extension_constructor
  | Float
  | Float32
  | Floatarray
  | Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Lazy_t of t
  (* CR sspies: Discuss lazy with multiple values inside. *)
  | Nativeint
  | String
  | Simd of simd_vec_split
  | Exception
  | Unboxed of unboxed

and array_kind =
  | Regular of t
  | Packed of t list

and unboxed =
  | Unboxed_float
  | Unboxed_float32
  | Unboxed_nativeint
  | Unboxed_int64
  | Unboxed_int32
  | Unboxed_int16
  | Unboxed_int8
  | Unboxed_simd of simd_vec_split

and simd_vec_split =
  (* 128 bit *)
  | Int8x16
  | Int16x8
  | Int32x4
  | Int64x2
  | Float16x8
  | Float32x4
  | Float64x2
  (* 256 bit *)
  | Int8x32
  | Int16x16
  | Int32x8
  | Int64x4
  | Float16x16
  | Float32x8
  | Float64x4
  (* 512 bit *)
  | Int8x64
  | Int16x32
  | Int32x16
  | Int64x8
  | Float16x32
  | Float32x16
  | Float64x8

val unknown : Runtime_layout.t -> t

val predef : predef -> t

val mixed_block_field : field_type:t -> label:'label -> 'label mixed_block_field

val map_mixed_block_field_label :
  ('a -> 'b) -> 'a mixed_block_field -> 'b mixed_block_field

val tuple : t list -> t

(** Create a constructor with tuple-style arguments for use in variants.

    Note that in [args], the order matters for the runtime memory layout. Ensure
    fields are in the correct order (i.e., values occur before non-values). *)
val constructor_with_tuple_arg :
  name:string -> args:unit mixed_block_field list -> constructor

(** Create a constructor with record-style arguments for use in variants.

    Note that in [args], the order matters for the runtime memory layout. Ensure
    fields are in the correct order (i.e., values occur before non-values). *)
val constructor_with_record_arg :
  name:string -> args:string mixed_block_field list -> constructor

val constructor_name : constructor -> string

val constructor_args : constructor -> string option mixed_block_field list

val variant : constructors -> t

val polymorphic_variant : constructors -> t

val variant_attribute_unboxed :
  constructor_name:string ->
  constructor_arg:string option mixed_block_field ->
  t

(** Attribute-unboxed record. Argument layout must have been flattened beforehand. *)
val record_attribute_unboxed : contents:string mixed_block_field -> t

(** Mixed record with value layout. Fields must have been unarized beforehand. *)
val record_mixed : string mixed_block_field list -> t

val func : t

val mu : t -> t

val rec_var : Shape.DeBruijn_index.t -> Runtime_layout.t -> t

val runtime_layout_of_unboxed : unboxed -> Runtime_layout.t

val runtime_layout : t -> Runtime_layout.t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val hash : t -> int

module Cache : Hashtbl.S with type key = t

val simd_vec_split_to_byte_size : simd_vec_split -> int
