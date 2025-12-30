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
module S = Shape
module Sort = Jkind_types.Sort
module Layout = Sort.Const

module DeBruijn_index = struct
  type t = int

  let create n =
    if n < 0
    then Misc.fatal_errorf "DeBruijn_index.create: negative index %d" n
    else n

  let move_under_binder n = n + 1

  let equal n1 n2 = Int.equal n1 n2

  let print fmt n = Format.fprintf fmt "%d" n
end

module DeBruijn_env = struct
  type 'a t = 'a list

  let empty = []

  let get_opt t ~de_bruijn_index = List.nth_opt t de_bruijn_index

  let push t x = x :: t

  let is_empty = function [] -> true | _ -> false
end

type 'a or_void =
  | Other of 'a
  | Void

let print_or_void f fmt = function
  | Other x -> f fmt x
  | Void -> Format.fprintf fmt "void"

let or_void_to_string to_string (t : 'a or_void) : string =
  match t with Other layout -> to_string layout | Void -> "void"

let erase_void (rs : 'a or_void list) : 'a list =
  List.filter_map (function Other s -> Some s | Void -> None) rs

module Runtime_layout = struct
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

  let size = function
    | Float64 -> 8
    | Float32 -> 4
    | Bits8 -> 1
    | Bits16 -> 2
    | Bits32 -> 4
    | Bits64 -> 8
    | Vec128 -> 16
    | Vec256 -> 32
    | Vec512 -> 64
    | Value -> Arch.size_addr
    | Word -> Arch.size_addr
    | Untagged_immediate ->
      Arch.size_addr (* CR sspies: Should be 63 bits, not 8 bytes *)

  let size_in_memory t = Int.max (size t) Arch.size_addr

  let of_base_layout (t : Sort.base) : t or_void =
    match t with
    | Void -> Void
    | Value -> Other Value
    | Untagged_immediate -> Other Untagged_immediate
    | Float64 -> Other Float64
    | Float32 -> Other Float32
    | Word -> Other Word
    | Bits8 -> Other Bits8
    | Bits16 -> Other Bits16
    | Bits32 -> Other Bits32
    | Bits64 -> Other Bits64
    | Vec128 -> Other Vec128
    | Vec256 -> Other Vec256
    | Vec512 -> Other Vec512

  let to_string (t : t) : string =
    match t with
    | Value -> "value"
    | Float64 -> "float64"
    | Float32 -> "float32"
    | Bits8 -> "bits8"
    | Bits16 -> "bits16"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"
    | Vec128 -> "vec128"
    | Vec256 -> "vec256"
    | Vec512 -> "vec512"
    | Word -> "word"
    | Untagged_immediate -> "untagged_immediate"

  let print fmt (t : t) = Format.pp_print_string fmt (to_string t)

  let to_base_layout : t -> Sort.base = function
    | Value -> Value
    | Float64 -> Float64
    | Float32 -> Float32
    | Bits8 -> Bits8
    | Bits16 -> Bits16
    | Bits32 -> Bits32
    | Bits64 -> Bits64
    | Vec128 -> Vec128
    | Vec256 -> Vec256
    | Vec512 -> Vec512
    | Word -> Word
    | Untagged_immediate -> Untagged_immediate

  let equal (t1 : t) (t2 : t) =
    match t1, t2 with
    | Value, Value
    | Float64, Float64
    | Float32, Float32
    | Bits8, Bits8
    | Bits16, Bits16
    | Bits32, Bits32
    | Bits64, Bits64
    | Vec128, Vec128
    | Vec256, Vec256
    | Vec512, Vec512
    | Word, Word
    | Untagged_immediate, Untagged_immediate ->
      true
    | ( ( Value | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Vec128
        | Vec256 | Vec512 | Word | Untagged_immediate ),
        _ ) ->
      false

  let hash : t -> int = function
    | Value -> 0
    | Float64 -> 1
    | Float32 -> 2
    | Bits8 -> 3
    | Bits16 -> 4
    | Bits32 -> 5
    | Bits64 -> 6
    | Vec128 -> 7
    | Vec256 -> 8
    | Vec512 -> 9
    | Word -> 10
    | Untagged_immediate -> 11
end

type t =
  { desc : desc;
    runtime_layout : Runtime_layout.t;
    hash : int
  }

and desc =
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
  | Rec_var of DeBruijn_index.t * Runtime_layout.t
(* The layout is part of [Unknown] and [Rec_var] to ensure that equality can be
   tested by simply comparing the descriptions. That is, the runtime_layout and
   hash fields are just precomputed from the desc field and carry no additional
   information. *)

and tuple_kind = Tuple_boxed

and variant_kind =
  | Variant_boxed
  | Variant_attribute_unboxed of Runtime_layout.t
  | Variant_polymorphic

and record_kind =
  | Record_attribute_unboxed of Runtime_layout.t
  | Record_mixed

and 'label mixed_block_field =
  { field_type : t;
    label : 'label
  }

and constructor =
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

let runtime_layout { runtime_layout; _ } = runtime_layout

let hash { hash; _ } = hash

(* Hash constants for desc constructors *)
let hash_unknown = 0

let hash_predef = 1

let hash_tuple = 2

let hash_variant = 3

let hash_record = 4

let hash_func = 5

let hash_mu = 6

let hash_rec_var = 7

(* Hash constants for predef constructors *)
let hash_predef_array = 0

let hash_predef_bytes = 1

let hash_predef_char = 2

let hash_predef_exception = 3

let hash_predef_extension_constructor = 4

let hash_predef_float = 5

let hash_predef_float32 = 6

let hash_predef_floatarray = 7

let hash_predef_int = 8

let hash_predef_int8 = 9

let hash_predef_int16 = 10

let hash_predef_int32 = 11

let hash_predef_int64 = 12

let hash_predef_lazy_t = 13

let hash_predef_nativeint = 14

let hash_predef_simd = 15

let hash_predef_string = 16

let hash_predef_unboxed = 17

let runtime_layout_of_simd_vec_split : simd_vec_split -> Runtime_layout.t =
  function
  | Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float16x8 | Float32x4 | Float64x2 ->
    Vec128
  | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float16x16 | Float32x8 | Float64x4
    ->
    Vec256
  | Int8x64 | Int16x32 | Int32x16 | Int64x8 | Float16x32 | Float32x16
  | Float64x8 ->
    Vec512

let runtime_layout_of_unboxed : unboxed -> Runtime_layout.t = function
  | Unboxed_float -> Float64
  | Unboxed_float32 -> Float32
  | Unboxed_nativeint -> Word
  | Unboxed_int64 -> Bits64
  | Unboxed_int32 -> Bits32
  | Unboxed_int16 -> Bits16
  | Unboxed_int8 -> Bits8
  | Unboxed_simd vec_split -> runtime_layout_of_simd_vec_split vec_split

let simd_vec_split_to_byte_size s =
  let layout = runtime_layout_of_simd_vec_split s in
  Runtime_layout.size layout

let runtime_layout_of_predef : predef -> Runtime_layout.t = function
  | Array _ | Bytes | Char | Extension_constructor | Exception | Float | Float32
  | Floatarray | Int | Int8 | Int16 | Int32 | Int64 | Lazy_t _ | Nativeint
  | Simd _ | String ->
    Value
  | Unboxed unboxed -> runtime_layout_of_unboxed unboxed

let hash_simd_vec_split : simd_vec_split -> int = function
  | Int8x16 -> 0
  | Int16x8 -> 1
  | Int32x4 -> 2
  | Int64x2 -> 3
  | Float16x8 -> 4
  | Float32x4 -> 5
  | Float64x2 -> 6
  | Int8x32 -> 7
  | Int16x16 -> 8
  | Int32x8 -> 9
  | Int64x4 -> 10
  | Float16x16 -> 11
  | Float32x8 -> 12
  | Float64x4 -> 13
  | Int8x64 -> 14
  | Int16x32 -> 15
  | Int32x16 -> 16
  | Int64x8 -> 17
  | Float16x32 -> 18
  | Float32x16 -> 19
  | Float64x8 -> 20

let hash_unboxed : unboxed -> int = function
  | Unboxed_float -> 0
  | Unboxed_float32 -> 1
  | Unboxed_nativeint -> 2
  | Unboxed_int64 -> 3
  | Unboxed_int32 -> 4
  | Unboxed_int16 -> 5
  | Unboxed_int8 -> 6
  | Unboxed_simd svs -> Hashtbl.hash (7, hash_simd_vec_split svs)

let hash_mixed_block_field (type label) (hash_label : label -> int)
    { field_type; label } =
  Hashtbl.hash (hash field_type, hash_label label)

let hash_constructor = function
  | Constructor_with_tuple_arg { name; args } ->
    Hashtbl.hash (0, name, List.map (hash_mixed_block_field (fun () -> 0)) args)
  | Constructor_with_record_arg { name; args } ->
    Hashtbl.hash (1, name, List.map (hash_mixed_block_field Hashtbl.hash) args)

let hash_array_kind = function
  | Regular s -> Hashtbl.hash (0, hash s)
  | Packed lst -> Hashtbl.hash (1, List.map hash lst)

let hash_predef predef =
  match predef with
  | Array kind -> Hashtbl.hash (hash_predef_array, hash_array_kind kind)
  | Bytes -> hash_predef_bytes
  | Char -> hash_predef_char
  | Extension_constructor -> hash_predef_extension_constructor
  | Float -> hash_predef_float
  | Float32 -> hash_predef_float32
  | Floatarray -> hash_predef_floatarray
  | Int -> hash_predef_int
  | Int8 -> hash_predef_int8
  | Int16 -> hash_predef_int16
  | Int32 -> hash_predef_int32
  | Int64 -> hash_predef_int64
  | Lazy_t s -> Hashtbl.hash (hash_predef_lazy_t, hash s)
  | Nativeint -> hash_predef_nativeint
  | String -> hash_predef_string
  | Simd svs -> Hashtbl.hash (hash_predef_simd, hash_simd_vec_split svs)
  | Exception -> hash_predef_exception
  | Unboxed unboxed -> Hashtbl.hash (hash_predef_unboxed, hash_unboxed unboxed)

let hash_variant_kind = function
  | Variant_boxed -> 0
  | Variant_attribute_unboxed layout ->
    Hashtbl.hash (1, Runtime_layout.hash layout)
  | Variant_polymorphic -> 2

let hash_record_kind = function
  | Record_attribute_unboxed layout -> Hashtbl.hash (0, layout)
  | Record_mixed -> 1

let hash_tuple_kind = function Tuple_boxed -> 0

let unknown layout =
  let desc = Unknown layout in
  { desc; runtime_layout = layout; hash = Hashtbl.hash (hash_unknown, layout) }

let predef p =
  let desc = Predef p in
  { desc;
    runtime_layout = runtime_layout_of_predef p;
    hash = Hashtbl.hash (hash_predef, hash_predef p)
  }

let mixed_block_field ~field_type ~label = { field_type; label }

let map_mixed_block_field_label f { field_type; label } =
  { field_type; label = f label }

let tuple args =
  let desc = Tuple { args; kind = Tuple_boxed } in
  { desc;
    runtime_layout = Value;
    hash =
      Hashtbl.hash (hash_tuple, hash_tuple_kind Tuple_boxed, List.map hash args)
  }

let constructor_with_tuple_arg ~name ~args =
  Constructor_with_tuple_arg { name; args }

let constructor_with_record_arg ~name ~args =
  Constructor_with_record_arg { name; args }

let variant constructors =
  let desc = Variant { constructors; kind = Variant_boxed } in
  { desc;
    runtime_layout = Value;
    hash =
      Hashtbl.hash
        ( hash_variant,
          hash_variant_kind Variant_boxed,
          List.map hash_constructor constructors )
  }

let polymorphic_variant constructors =
  let desc = Variant { constructors; kind = Variant_polymorphic } in
  { desc;
    runtime_layout = Value;
    hash =
      Hashtbl.hash
        ( hash_variant,
          hash_variant_kind Variant_polymorphic,
          List.map hash_constructor constructors )
  }

let variant_attribute_unboxed ~constructor_name
    ~(constructor_arg : string option mixed_block_field) =
  let layout = runtime_layout constructor_arg.field_type in
  let constructor : constructor =
    match constructor_arg.label with
    | None ->
      Constructor_with_tuple_arg
        { name = constructor_name;
          args = [{ field_type = constructor_arg.field_type; label = () }]
        }
    | Some label ->
      Constructor_with_record_arg
        { name = constructor_name;
          args = [{ field_type = constructor_arg.field_type; label }]
        }
  in
  let desc =
    Variant
      { constructors = [constructor]; kind = Variant_attribute_unboxed layout }
  in
  { desc;
    runtime_layout = layout;
    hash =
      Hashtbl.hash
        ( hash_variant,
          hash_variant_kind (Variant_attribute_unboxed layout),
          List.map hash_constructor [constructor] )
  }

(* Using mixed records here, because they generalize regular records. *)
let record fields =
  let desc = Record { fields; kind = Record_mixed } in
  { desc;
    runtime_layout = Value;
    hash =
      Hashtbl.hash
        ( hash_record,
          hash_record_kind Record_mixed,
          List.map (hash_mixed_block_field Hashtbl.hash) fields )
  }

let record_attribute_unboxed ~contents =
  let layout = runtime_layout contents.field_type in
  let desc =
    Record { fields = [contents]; kind = Record_attribute_unboxed layout }
  in
  { desc;
    runtime_layout = layout;
    hash =
      Hashtbl.hash
        ( hash_record,
          hash_record_kind (Record_attribute_unboxed layout),
          List.map (hash_mixed_block_field Hashtbl.hash) [contents] )
  }

let record_mixed fields =
  let desc = Record { fields; kind = Record_mixed } in
  { desc;
    runtime_layout = Value;
    hash =
      Hashtbl.hash
        ( hash_record,
          hash_record_kind Record_mixed,
          List.map (hash_mixed_block_field Hashtbl.hash) fields )
  }

let func =
  let desc = Func in
  { desc; runtime_layout = Value; hash = Hashtbl.hash hash_func }

let mu shape =
  let desc = Mu shape in
  { desc;
    runtime_layout = runtime_layout shape;
    hash = Hashtbl.hash (hash_mu, hash shape)
  }

let rec_var var ly =
  let desc = Rec_var (var, ly) in
  { desc; runtime_layout = ly; hash = Hashtbl.hash (hash_rec_var, (var, ly)) }

let print_runtime_layout fmt (t : Runtime_layout.t) =
  Format.pp_print_string fmt (Runtime_layout.to_string t)

let print_simd_vec_split fmt svs =
  let s =
    match svs with
    | Int8x16 -> "int8x16"
    | Int16x8 -> "int16x8"
    | Int32x4 -> "int32x4"
    | Int64x2 -> "int64x2"
    | Float16x8 -> "float16x8"
    | Float32x4 -> "float32x4"
    | Float64x2 -> "float64x2"
    | Int8x32 -> "int8x32"
    | Int16x16 -> "int16x16"
    | Int32x8 -> "int32x8"
    | Int64x4 -> "int64x4"
    | Float16x16 -> "float16x16"
    | Float32x8 -> "float32x8"
    | Float64x4 -> "float64x4"
    | Int8x64 -> "int8x64"
    | Int16x32 -> "int16x32"
    | Int32x16 -> "int32x16"
    | Int64x8 -> "int64x8"
    | Float16x32 -> "float16x32"
    | Float32x16 -> "float32x16"
    | Float64x8 -> "float64x8"
  in
  Format.pp_print_string fmt s

let print_unboxed fmt unb =
  let s =
    match unb with
    | Unboxed_float -> "float#"
    | Unboxed_float32 -> "float32#"
    | Unboxed_nativeint -> "nativeint#"
    | Unboxed_int64 -> "int64#"
    | Unboxed_int32 -> "int32#"
    | Unboxed_int16 -> "int16#"
    | Unboxed_int8 -> "int8#"
    | Unboxed_simd svs -> Format.asprintf "%a#" print_simd_vec_split svs
  in
  Format.pp_print_string fmt s

let rec print fmt { desc } =
  match desc with
  | Unknown layout ->
    Format.fprintf fmt "Unknown(%a)" print_runtime_layout layout
  | Predef predef -> print_predef fmt predef
  | Tuple { args; kind = Tuple_boxed } ->
    Format.fprintf fmt "Tuple(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         print)
      args
  | Variant { constructors; kind } ->
    let kind_str =
      match kind with
      | Variant_boxed -> "boxed"
      | Variant_attribute_unboxed _ -> "attr_unboxed"
      | Variant_polymorphic -> "poly"
    in
    let print_constructor fmt = function
      | Constructor_with_tuple_arg { name; args } ->
        Format.fprintf fmt "%s (%a)" name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt { field_type; label = () } ->
               Format.fprintf fmt "%a @ %a" print field_type
                 print_runtime_layout
                 (runtime_layout field_type)))
          args
      | Constructor_with_record_arg { name; args } ->
        Format.fprintf fmt "%s { %a }" name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt { field_type; label } ->
               Format.fprintf fmt "%s: %a @ %a" label print field_type
                 print_runtime_layout
                 (runtime_layout field_type)))
          args
    in
    Format.fprintf fmt "Variant(%s, [%a])" kind_str
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ")
         print_constructor)
      constructors
  | Record { fields; kind } ->
    let kind_str =
      match kind with
      | Record_attribute_unboxed _ -> "attr_unboxed"
      | Record_mixed -> "mixed"
    in
    let print_record_field fmt { field_type; label } =
      Format.fprintf fmt "%s: %a @ %a" label print field_type
        print_runtime_layout
        (runtime_layout field_type)
    in
    Format.fprintf fmt "Record(%s, { %a })" kind_str
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         print_record_field)
      fields
  | Func -> Format.fprintf fmt "Func"
  | Mu shape -> Format.fprintf fmt "Mu(%a)" print shape
  | Rec_var (idx, ly) ->
    Format.fprintf fmt "Rec_var(%a, %a)" DeBruijn_index.print idx
      Runtime_layout.print ly

and print_predef fmt p =
  match p with
  | Array (Regular elem) -> Format.fprintf fmt "%a array" print elem
  | Array (Packed elems) ->
    Format.fprintf fmt "{%a} array"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         (fun fmt shape ->
           let layout = runtime_layout shape in
           let elem_size = Runtime_layout.size_in_memory layout in
           Format.fprintf fmt "%a[%d bytes]" print shape elem_size))
      elems
  | Bytes -> Format.pp_print_string fmt "bytes"
  | Char -> Format.pp_print_string fmt "char"
  | Extension_constructor -> Format.pp_print_string fmt "extension_constructor"
  | Float -> Format.pp_print_string fmt "float"
  | Float32 -> Format.pp_print_string fmt "float32"
  | Floatarray -> Format.pp_print_string fmt "floatarray"
  | Int -> Format.pp_print_string fmt "int"
  | Int8 -> Format.pp_print_string fmt "int8"
  | Int16 -> Format.pp_print_string fmt "int16"
  | Int32 -> Format.pp_print_string fmt "int32"
  | Int64 -> Format.pp_print_string fmt "int64"
  | Lazy_t elem -> Format.fprintf fmt "%a lazy_t" print elem
  | Nativeint -> Format.pp_print_string fmt "nativeint"
  | String -> Format.pp_print_string fmt "string"
  | Simd svs -> Format.fprintf fmt "%a" print_simd_vec_split svs
  | Exception -> Format.pp_print_string fmt "exn"
  | Unboxed unb -> Format.fprintf fmt "%a" print_unboxed unb

let equal_simd_vec_split svs1 svs2 =
  match svs1, svs2 with
  | Int8x16, Int8x16
  | Int16x8, Int16x8
  | Int32x4, Int32x4
  | Int64x2, Int64x2
  | Float16x8, Float16x8
  | Float32x4, Float32x4
  | Float64x2, Float64x2
  | Int8x32, Int8x32
  | Int16x16, Int16x16
  | Int32x8, Int32x8
  | Int64x4, Int64x4
  | Float16x16, Float16x16
  | Float32x8, Float32x8
  | Float64x4, Float64x4
  | Int8x64, Int8x64
  | Int16x32, Int16x32
  | Int32x16, Int32x16
  | Int64x8, Int64x8
  | Float16x32, Float16x32
  | Float32x16, Float32x16
  | Float64x8, Float64x8 ->
    true
  | ( ( Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float16x8 | Float32x4
      | Float64x2 | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float16x16
      | Float32x8 | Float64x4 | Int8x64 | Int16x32 | Int32x16 | Int64x8
      | Float16x32 | Float32x16 | Float64x8 ),
      _ ) ->
    false

let equal_unboxed u1 u2 =
  match u1, u2 with
  | Unboxed_float, Unboxed_float
  | Unboxed_float32, Unboxed_float32
  | Unboxed_nativeint, Unboxed_nativeint
  | Unboxed_int64, Unboxed_int64
  | Unboxed_int32, Unboxed_int32
  | Unboxed_int16, Unboxed_int16
  | Unboxed_int8, Unboxed_int8 ->
    true
  | Unboxed_simd svs1, Unboxed_simd svs2 -> equal_simd_vec_split svs1 svs2
  | ( ( Unboxed_float | Unboxed_float32 | Unboxed_nativeint | Unboxed_int64
      | Unboxed_int32 | Unboxed_int16 | Unboxed_int8 | Unboxed_simd _ ),
      _ ) ->
    false

let equal_tuple_kind kind1 kind2 =
  match kind1, kind2 with Tuple_boxed, Tuple_boxed -> true

let equal_variant_kind kind1 kind2 =
  match kind1, kind2 with
  | Variant_boxed, Variant_boxed -> true
  | Variant_attribute_unboxed layout1, Variant_attribute_unboxed layout2 ->
    Runtime_layout.equal layout1 layout2
  | Variant_polymorphic, Variant_polymorphic -> true
  | (Variant_boxed | Variant_attribute_unboxed _ | Variant_polymorphic), _ ->
    false

let equal_record_kind kind1 kind2 =
  match kind1, kind2 with
  | Record_mixed, Record_mixed -> true
  | Record_attribute_unboxed layout1, Record_attribute_unboxed layout2 ->
    Runtime_layout.equal layout1 layout2
  | (Record_attribute_unboxed _ | Record_mixed), _ -> false

let rec equal { desc = desc1 } { desc = desc2 } =
  match desc1, desc2 with
  | Unknown layout1, Unknown layout2 -> Runtime_layout.equal layout1 layout2
  | Predef p1, Predef p2 -> equal_predef p1 p2
  | Tuple { args = args1; kind = kind1 }, Tuple { args = args2; kind = kind2 }
    ->
    equal_tuple_kind kind1 kind2 && List.equal equal args1 args2
  | ( Variant { constructors = constrs1; kind = kind1 },
      Variant { constructors = constrs2; kind = kind2 } ) ->
    equal_variant_kind kind1 kind2
    && List.equal equal_constructor constrs1 constrs2
  | ( Record { fields = fields1; kind = kind1 },
      Record { fields = fields2; kind = kind2 } ) ->
    equal_record_kind kind1 kind2
    && List.equal equal_record_field fields1 fields2
  | Func, Func -> true
  | Mu shape1, Mu shape2 -> equal shape1 shape2
  | Rec_var (idx1, ly1), Rec_var (idx2, ly2) ->
    DeBruijn_index.equal idx1 idx2 && Runtime_layout.equal ly1 ly2
  | ( ( Unknown _ | Predef _ | Tuple _ | Variant _ | Record _ | Func | Mu _
      | Rec_var _ ),
      _ ) ->
    false

and equal_tuple_field { field_type = type1; label = () }
    { field_type = type2; label = () } =
  equal type1 type2

and equal_record_field { field_type = type1; label = label1 }
    { field_type = type2; label = label2 } =
  equal type1 type2 && String.equal label1 label2

and equal_constructor c1 c2 =
  match c1, c2 with
  | ( Constructor_with_tuple_arg { name = name1; args = args1 },
      Constructor_with_tuple_arg { name = name2; args = args2 } ) ->
    String.equal name1 name2 && List.equal equal_tuple_field args1 args2
  | ( Constructor_with_record_arg { name = name1; args = args1 },
      Constructor_with_record_arg { name = name2; args = args2 } ) ->
    String.equal name1 name2 && List.equal equal_record_field args1 args2
  | Constructor_with_tuple_arg _, Constructor_with_record_arg _ -> false
  | Constructor_with_record_arg _, Constructor_with_tuple_arg _ -> false

and equal_array_kind k1 k2 =
  match k1, k2 with
  | Regular s1, Regular s2 -> equal s1 s2
  | Packed lst1, Packed lst2 -> List.equal equal lst1 lst2
  | (Regular _ | Packed _), _ -> false

and equal_predef p1 p2 =
  match p1, p2 with
  | Array k1, Array k2 -> equal_array_kind k1 k2
  | Bytes, Bytes
  | Char, Char
  | Extension_constructor, Extension_constructor
  | Float, Float
  | Float32, Float32
  | Floatarray, Floatarray
  | Int, Int
  | Int8, Int8
  | Int16, Int16
  | Int32, Int32
  | Int64, Int64
  | Nativeint, Nativeint
  | String, String
  | Exception, Exception ->
    true
  | Lazy_t s1, Lazy_t s2 -> equal s1 s2
  | Simd svs1, Simd svs2 -> equal_simd_vec_split svs1 svs2
  | Unboxed u1, Unboxed u2 -> equal_unboxed u1 u2
  | ( ( Array _ | Bytes | Char | Extension_constructor | Float | Float32
      | Floatarray | Int | Int8 | Int16 | Int32 | Int64 | Nativeint | String
      | Lazy_t _ | Simd _ | Exception | Unboxed _ ),
      _ ) ->
    false

let constructor_name = function
  | Constructor_with_tuple_arg { name; _ } -> name
  | Constructor_with_record_arg { name; _ } -> name

let constructor_args = function
  | Constructor_with_tuple_arg { args; _ } ->
    List.map
      (fun { field_type; label = () } -> { field_type; label = None })
      args
  | Constructor_with_record_arg { args; _ } ->
    List.map
      (fun { field_type; label } -> { field_type; label = Some label })
      args

module Cache = Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = hash
end)
