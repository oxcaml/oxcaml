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

module Runtime_shape : sig
  (** Runtime shape (which has a runtime layout) *)
  type t = private
    { rs_desc : desc;
      rs_runtime_layout : Runtime_layout.t;
      rs_hash : int
    }

  and desc = private
    | Unknown of Runtime_layout.t
    | Predef of predef
    | Tuple of
        { tuple_args : t list;
          tuple_kind : tuple_kind
        }
    | Variant of
        { variant_constructors : constructors;
          variant_kind : variant_kind
        }
    | Record of
        { record_fields : string mixed_block_field list;
          record_kind : record_kind
        }
    | Func
    | Mu of t
    | Rec_var of Shape.DeBruijn_index.t * Runtime_layout.t
  (* CR sspies: Use regular identifiers beforehad and only switch to DeBruijn at
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
    { mbf_type : t;
      mbf_element : Runtime_layout.t;
      mbf_label : 'label
    }

  and constructor = private
    | Tuple_constructor of
        { constr_name : string;
          constr_args : unit mixed_block_field list
        }
    | Record_constructor of
        { constr_name : string;
          constr_args : string mixed_block_field list
        }

  and constructors = constructor list

  and predef = private
    | Array_regular of t
    | Array_packed of t list
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

  and unboxed = private
    | Unboxed_float
    | Unboxed_float32
    | Unboxed_nativeint
    | Unboxed_int64
    | Unboxed_int32
    | Unboxed_int16
    | Unboxed_int8
    | Unboxed_simd of simd_vec_split

  and simd_vec_split = private
    (* 128 bit *)
    | Int8x16
    | Int16x8
    | Int32x4
    | Int64x2
    | Float32x4
    | Float64x2
    (* 256 bit *)
    | Int8x32
    | Int16x16
    | Int32x8
    | Int64x4
    | Float32x8
    | Float64x4
    (* 512 bit *)
    | Int8x64
    | Int16x32
    | Int32x16
    | Int64x8
    | Float32x16
    | Float64x8

  (* Runtime shape constructors *)

  val unknown : Runtime_layout.t -> t

  val predef : predef -> t

  (** regular, boxed tuple with value layout *)
  val tuple : t list -> t

  (** regular variant with value layout *)
  val variant : constructors -> t

  (** polymorphic variant with value layout *)
  val polymorphic_variant : constructors -> t

  (** attribute-unboxed variant *)
  val variant_attribute_unboxed :
    constructor_name:string ->
    constructor_arg:string option mixed_block_field ->
    t

  (** attribute-unboxed record; argument layout must have been flattened beforehand. *)
  val record_attribute_unboxed : contents:string mixed_block_field -> t

  (** mixed record with value layout. Fields must have been unarized beforehand. *)
  val record_mixed : string mixed_block_field list -> t

  val func : t

  val mu : t -> t

  val rec_var : Shape.DeBruijn_index.t -> Runtime_layout.t -> t

  (* Constructor projection functions *)

  val constructor_name : constructor -> string

  val constructor_args : constructor -> string option mixed_block_field list

  (* Runtime shape utility functions *)

  val runtime_layout_of_unboxed : unboxed -> Runtime_layout.t

  val to_runtime_layout : t -> Runtime_layout.t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  module Cache : Hashtbl.S with type key = t

  val simd_vec_split_to_byte_size : simd_vec_split -> int
end

module Virtual_shape : sig
  type t = private
    { vs_desc : desc;
      vs_layout : Layout.t;
      vs_hash : int
    }

  and desc = private
    | Runtime of Runtime_shape.t
    | Void
    | UnboxedProduct of
        { unboxed_product_kind : unboxed_product_kind;
          unboxed_product_components : t list
        }

  and unboxed_product_kind = private
    | Unboxed_record of string list
    | Unboxed_tuple

  val runtime : Runtime_shape.t -> t

  (** unboxed tuple with product layout *)
  val unboxed_tuple : t list -> t

  (** unboxed record with product layout *)
  val record_unboxed : (string * t) list -> t

  val void : t

  (* Virtual shape utility functions *)

  val to_layout : t -> Layout.t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int
end

val runtime_shape : Virtual_shape.t -> Runtime_shape.t option

val type_shape_to_dwarf_shape : Shape.t -> Layout.t -> Virtual_shape.t

(* for unarization *)
val flatten_virtual_shape : Virtual_shape.t -> Runtime_shape.t or_void list
