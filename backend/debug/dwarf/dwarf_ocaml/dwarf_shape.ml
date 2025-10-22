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

type 'a or_void =
  | Other of 'a
  | Void

let print_or_void f fmt = function
  | Other x -> f fmt x
  | Void -> Format.fprintf fmt "void"

let or_void_to_string to_string (t : 'a or_void) : string =
  match t with Other layout -> to_string layout | Void -> "void"

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

module Runtime_shape = struct
  type t =
    { rs_desc : desc;
      rs_runtime_layout : Runtime_layout.t;
      rs_hash : int
    }

  and desc =
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
  (* The layout is part of [Unknown] and [Rec_var] to ensure that equality can
     be tested by simply comparing the descriptions. That is, the runtime_layout
     and hash fields are just precomputed from the desc field and carry no
     additional information. *)

  and tuple_kind = Tuple_boxed

  and variant_kind =
    | Variant_boxed
    | Variant_attribute_unboxed of Runtime_layout.t
    | Variant_polymorphic

  and record_kind =
    | Record_attribute_unboxed of Runtime_layout.t
    | Record_mixed

  and 'label mixed_block_field =
    { mbf_type : t;
      mbf_element : Runtime_layout.t;
      mbf_label : 'label
    }

  and constructor =
    | Tuple_constructor of
        { constr_name : string;
          constr_args : unit mixed_block_field list
        }
    | Record_constructor of
        { constr_name : string;
          constr_args : string mixed_block_field list
        }

  and constructors = constructor list

  and predef =
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
    | Nativeint
    | String
    | Simd of simd_vec_split
    | Exception
    | Unboxed of unboxed

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

  let to_runtime_layout { rs_runtime_layout; _ } = rs_runtime_layout

  let hash { rs_hash; _ } = rs_hash

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
  let hash_predef_array_regular = 0

  let hash_predef_array_packed = 1

  let hash_predef_bytes = 2

  let hash_predef_char = 3

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

  let hash_predef_string = 15

  let hash_predef_simd = 16

  let hash_predef_exception = 17

  let hash_predef_unboxed = 18

  let runtime_layout_of_simd_vec_split : simd_vec_split -> Runtime_layout.t =
    function
    | Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2 -> Vec128
    | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float32x8 | Float64x4 -> Vec256
    | Int8x64 | Int16x32 | Int32x16 | Int64x8 | Float32x16 | Float64x8 -> Vec512

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
    | Array_regular _ | Array_packed _ | Bytes | Char | Extension_constructor
    | Exception | Float | Float32 | Floatarray | Int | Int8 | Int16 | Int32
    | Int64 | Lazy_t _ | Nativeint | Simd _ | String ->
      Value
    | Unboxed unboxed -> runtime_layout_of_unboxed unboxed

  let hash_simd_vec_split : simd_vec_split -> int = function
    | Int8x16 -> 0
    | Int16x8 -> 1
    | Int32x4 -> 2
    | Int64x2 -> 3
    | Float32x4 -> 4
    | Float64x2 -> 5
    | Int8x32 -> 6
    | Int16x16 -> 7
    | Int32x8 -> 8
    | Int64x4 -> 9
    | Float32x8 -> 10
    | Float64x4 -> 11
    | Int8x64 -> 12
    | Int16x32 -> 13
    | Int32x16 -> 14
    | Int64x8 -> 15
    | Float32x16 -> 16
    | Float64x8 -> 17

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
      { mbf_type; mbf_element; mbf_label } =
    Hashtbl.hash
      (hash mbf_type, Runtime_layout.hash mbf_element, hash_label mbf_label)

  let hash_constructor = function
    | Tuple_constructor { constr_name; constr_args } ->
      Hashtbl.hash
        ( 0,
          constr_name,
          List.map (hash_mixed_block_field (fun () -> 0)) constr_args )
    | Record_constructor { constr_name; constr_args } ->
      Hashtbl.hash
        ( 1,
          constr_name,
          List.map (hash_mixed_block_field Hashtbl.hash) constr_args )

  let hash_predef predef =
    match predef with
    | Array_regular s -> Hashtbl.hash (hash_predef_array_regular, hash s)
    | Array_packed lst ->
      Hashtbl.hash (hash_predef_array_packed, List.map hash lst)
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

  (* Runtime shape constructors *)

  let unknown layout =
    let rs_desc = Unknown layout in
    { rs_desc;
      rs_runtime_layout = layout;
      rs_hash = Hashtbl.hash (hash_unknown, layout)
    }

  let predef p =
    let rs_desc = Predef p in
    { rs_desc;
      rs_runtime_layout = runtime_layout_of_predef p;
      rs_hash = Hashtbl.hash (hash_predef, hash_predef p)
    }

  let tuple args =
    let rs_desc = Tuple { tuple_args = args; tuple_kind = Tuple_boxed } in
    { rs_desc;
      rs_runtime_layout = Value;
      rs_hash = Hashtbl.hash (hash_tuple, Tuple_boxed, List.map hash args)
    }

  let variant constructors =
    let rs_desc =
      Variant
        { variant_constructors = constructors; variant_kind = Variant_boxed }
    in
    { rs_desc;
      rs_runtime_layout = Value;
      rs_hash =
        Hashtbl.hash
          ( hash_variant,
            hash_variant_kind Variant_boxed,
            List.map hash_constructor constructors )
    }

  let polymorphic_variant constructors =
    let rs_desc =
      Variant
        { variant_constructors = constructors;
          variant_kind = Variant_polymorphic
        }
    in
    { rs_desc;
      rs_runtime_layout = Value;
      rs_hash =
        Hashtbl.hash
          ( hash_variant,
            hash_variant_kind Variant_polymorphic,
            List.map hash_constructor constructors )
    }

  let variant_attribute_unboxed ~constructor_name
      ~(constructor_arg : string option mixed_block_field) =
    let layout = constructor_arg.mbf_element in
    let constructor : constructor =
      match constructor_arg.mbf_label with
      | None ->
        Tuple_constructor
          { constr_name = constructor_name;
            constr_args =
              [ { mbf_type = constructor_arg.mbf_type;
                  mbf_element = constructor_arg.mbf_element;
                  mbf_label = ()
                } ]
          }
      | Some label ->
        Record_constructor
          { constr_name = constructor_name;
            constr_args =
              [ { mbf_type = constructor_arg.mbf_type;
                  mbf_element = constructor_arg.mbf_element;
                  mbf_label = label
                } ]
          }
    in
    let rs_desc =
      Variant
        { variant_constructors = [constructor];
          variant_kind = Variant_attribute_unboxed layout
        }
    in
    { rs_desc;
      rs_runtime_layout = layout;
      rs_hash =
        Hashtbl.hash
          ( hash_variant,
            hash_variant_kind (Variant_attribute_unboxed layout),
            List.map hash_constructor [constructor] )
    }

  (* Using mixed records here, because they generalize regular records. *)
  let record fields =
    let rs_desc =
      Record { record_fields = fields; record_kind = Record_mixed }
    in
    { rs_desc;
      rs_runtime_layout = Value;
      rs_hash =
        Hashtbl.hash
          ( hash_record,
            hash_record_kind Record_mixed,
            List.map (hash_mixed_block_field Hashtbl.hash) fields )
    }

  let record_attribute_unboxed ~contents =
    let layout = contents.mbf_element in
    let rs_desc =
      Record
        { record_fields = [contents];
          record_kind = Record_attribute_unboxed layout
        }
    in
    { rs_desc;
      rs_runtime_layout = layout;
      rs_hash =
        Hashtbl.hash
          ( hash_record,
            hash_record_kind (Record_attribute_unboxed layout),
            List.map (hash_mixed_block_field Hashtbl.hash) [contents] )
    }

  let record_mixed fields =
    let rs_desc =
      Record { record_fields = fields; record_kind = Record_mixed }
    in
    { rs_desc;
      rs_runtime_layout = Value;
      rs_hash =
        Hashtbl.hash
          ( hash_record,
            hash_record_kind Record_mixed,
            List.map (hash_mixed_block_field Hashtbl.hash) fields )
    }

  let func =
    let rs_desc = Func in
    { rs_desc; rs_runtime_layout = Value; rs_hash = Hashtbl.hash hash_func }

  let mu shape =
    let rs_desc = Mu shape in
    { rs_desc;
      rs_runtime_layout = to_runtime_layout shape;
      rs_hash = Hashtbl.hash (hash_mu, hash shape)
    }

  let rec_var var ly =
    let rs_desc = Rec_var (var, ly) in
    { rs_desc;
      rs_runtime_layout = ly;
      rs_hash = Hashtbl.hash (hash_rec_var, (var, ly))
    }

  let print_runtime_layout fmt (t : Runtime_layout.t) =
    Format.pp_print_string fmt (Runtime_layout.to_string t)

  let print_simd_vec_split fmt svs =
    let s =
      match svs with
      | Int8x16 -> "int8x16"
      | Int16x8 -> "int16x8"
      | Int32x4 -> "int32x4"
      | Int64x2 -> "int64x2"
      | Float32x4 -> "float32x4"
      | Float64x2 -> "float64x2"
      | Int8x32 -> "int8x32"
      | Int16x16 -> "int16x16"
      | Int32x8 -> "int32x8"
      | Int64x4 -> "int64x4"
      | Float32x8 -> "float32x8"
      | Float64x4 -> "float64x4"
      | Int8x64 -> "int8x64"
      | Int16x32 -> "int16x32"
      | Int32x16 -> "int32x16"
      | Int64x8 -> "int64x8"
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

  let rec print fmt { rs_desc } =
    match rs_desc with
    | Unknown layout ->
      Format.fprintf fmt "Unknown(%a)" print_runtime_layout layout
    | Predef predef -> print_predef fmt predef
    | Tuple { tuple_args; tuple_kind = Tuple_boxed } ->
      Format.fprintf fmt "Tuple(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           print)
        tuple_args
    | Variant { variant_constructors; variant_kind } ->
      let kind_str =
        match variant_kind with
        | Variant_boxed -> "boxed"
        | Variant_attribute_unboxed _ -> "attr_unboxed"
        | Variant_polymorphic -> "poly"
      in
      let print_constructor fmt = function
        | Tuple_constructor { constr_name; constr_args } ->
          Format.fprintf fmt "%s (%a)" constr_name
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               (fun fmt { mbf_type; mbf_element; mbf_label = () } ->
                 Format.fprintf fmt "%a @ %a" print mbf_type
                   print_runtime_layout mbf_element))
            constr_args
        | Record_constructor { constr_name; constr_args } ->
          Format.fprintf fmt "%s { %a }" constr_name
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
               (fun fmt { mbf_type; mbf_element; mbf_label } ->
                 Format.fprintf fmt "%s: %a @ %a" mbf_label print mbf_type
                   print_runtime_layout mbf_element))
            constr_args
      in
      Format.fprintf fmt "Variant(%s, [%a])" kind_str
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ")
           print_constructor)
        variant_constructors
    | Record { record_fields; record_kind } ->
      let kind_str =
        match record_kind with
        | Record_attribute_unboxed _ -> "attr_unboxed"
        | Record_mixed -> "mixed"
      in
      let print_record_field fmt { mbf_type; mbf_element; mbf_label } =
        Format.fprintf fmt "%s: %a @ %a" mbf_label print mbf_type
          print_runtime_layout mbf_element
      in
      Format.fprintf fmt "Record(%s, { %a })" kind_str
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           print_record_field)
        record_fields
    | Func -> Format.fprintf fmt "Func"
    | Mu shape -> Format.fprintf fmt "Mu(%a)" print shape
    | Rec_var (idx, ly) ->
      Format.fprintf fmt "Rec_var(%a, %a)" Shape.DeBruijn_index.print idx
        Runtime_layout.print ly

  and print_predef fmt p =
    match p with
    | Array_regular elem -> Format.fprintf fmt "%a array" print elem
    | Array_packed elems ->
      Format.fprintf fmt "{%a} array"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt shape ->
             let layout = to_runtime_layout shape in
             let elem_size = Runtime_layout.size_in_memory layout in
             Format.fprintf fmt "%a[%d bytes]" print shape elem_size))
        elems
    | Bytes -> Format.pp_print_string fmt "bytes"
    | Char -> Format.pp_print_string fmt "char"
    | Extension_constructor ->
      Format.pp_print_string fmt "extension_constructor"
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
    | Float32x4, Float32x4
    | Float64x2, Float64x2
    | Int8x32, Int8x32
    | Int16x16, Int16x16
    | Int32x8, Int32x8
    | Int64x4, Int64x4
    | Float32x8, Float32x8
    | Float64x4, Float64x4
    | Int8x64, Int8x64
    | Int16x32, Int16x32
    | Int32x16, Int32x16
    | Int64x8, Int64x8
    | Float32x16, Float32x16
    | Float64x8, Float64x8 ->
      true
    | ( ( Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2
        | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float32x8 | Float64x4
        | Int8x64 | Int16x32 | Int32x16 | Int64x8 | Float32x16 | Float64x8 ),
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

  let rec equal { rs_desc = desc1 } { rs_desc = desc2 } =
    match desc1, desc2 with
    | Unknown layout1, Unknown layout2 -> Runtime_layout.equal layout1 layout2
    | Predef p1, Predef p2 -> equal_predef p1 p2
    | ( Tuple { tuple_args = args1; tuple_kind = kind1 },
        Tuple { tuple_args = args2; tuple_kind = kind2 } ) ->
      equal_tuple_kind kind1 kind2 && List.equal equal args1 args2
    | ( Variant { variant_constructors = constrs1; variant_kind = kind1 },
        Variant { variant_constructors = constrs2; variant_kind = kind2 } ) ->
      equal_variant_kind kind1 kind2
      && List.equal equal_constructor constrs1 constrs2
    | ( Record { record_fields = fields1; record_kind = kind1 },
        Record { record_fields = fields2; record_kind = kind2 } ) ->
      equal_record_kind kind1 kind2
      && List.equal equal_record_field fields1 fields2
    | Func, Func -> true
    | Mu shape1, Mu shape2 -> equal shape1 shape2
    | Rec_var (idx1, ly1), Rec_var (idx2, ly2) ->
      Shape.DeBruijn_index.equal idx1 idx2 && Runtime_layout.equal ly1 ly2
    | ( ( Unknown _ | Predef _ | Tuple _ | Variant _ | Record _ | Func | Mu _
        | Rec_var _ ),
        _ ) ->
      false

  and equal_tuple_field
      { mbf_type = type1; mbf_element = elem1; mbf_label = () }
      { mbf_type = type2; mbf_element = elem2; mbf_label = () } =
    equal type1 type2 && Runtime_layout.equal elem1 elem2

  and equal_record_field
      { mbf_type = type1; mbf_element = elem1; mbf_label = label1 }
      { mbf_type = type2; mbf_element = elem2; mbf_label = label2 } =
    equal type1 type2
    && Runtime_layout.equal elem1 elem2
    && String.equal label1 label2

  and equal_constructor c1 c2 =
    match c1, c2 with
    | ( Tuple_constructor { constr_name = name1; constr_args = args1 },
        Tuple_constructor { constr_name = name2; constr_args = args2 } ) ->
      String.equal name1 name2 && List.equal equal_tuple_field args1 args2
    | ( Record_constructor { constr_name = name1; constr_args = args1 },
        Record_constructor { constr_name = name2; constr_args = args2 } ) ->
      String.equal name1 name2 && List.equal equal_record_field args1 args2
    | Tuple_constructor _, Record_constructor _ -> false
    | Record_constructor _, Tuple_constructor _ -> false

  and equal_predef p1 p2 =
    match p1, p2 with
    | Array_regular s1, Array_regular s2 -> equal s1 s2
    | Array_packed lst1, Array_packed lst2 -> List.equal equal lst1 lst2
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
    | ( ( Array_regular _ | Array_packed _ | Bytes | Char
        | Extension_constructor | Float | Float32 | Floatarray | Int | Int8
        | Int16 | Int32 | Int64 | Nativeint | String | Lazy_t _ | Simd _
        | Exception | Unboxed _ ),
        _ ) ->
      false

  let constructor_name = function
    | Tuple_constructor { constr_name; _ } -> constr_name
    | Record_constructor { constr_name; _ } -> constr_name

  let constructor_args = function
    | Tuple_constructor { constr_args; _ } ->
      List.map
        (fun { mbf_type; mbf_element; mbf_label = () } ->
          { mbf_type; mbf_element; mbf_label = None })
        constr_args
    | Record_constructor { constr_args; _ } ->
      List.map
        (fun { mbf_type; mbf_element; mbf_label } ->
          { mbf_type; mbf_element; mbf_label = Some mbf_label })
        constr_args

  module Cache = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)
end

(* Virtual shape types *)

module Virtual_shape = struct
  type t =
    { vs_desc : desc;
      vs_layout : Layout.t;
      vs_hash : int
    }

  and desc =
    | Runtime of Runtime_shape.t
    | Void
    | UnboxedProduct of
        { unboxed_product_kind : unboxed_product_kind;
          unboxed_product_components : t list
        }

  and unboxed_product_kind =
    | Unboxed_record of string list
    | Unboxed_tuple

  (* Hash constants for Virtual_shape desc constructors *)
  let hash_runtime = 0

  let hash_void = 1

  let hash_unboxed_product = 2

  (* Virtual shape hash functions *)

  let hash_unboxed_product_kind : unboxed_product_kind -> int = function
    | Unboxed_record names -> Hashtbl.hash (0, List.map Hashtbl.hash names)
    | Unboxed_tuple -> 1

  let hash { vs_hash; _ } = vs_hash

  let to_layout { vs_layout; _ } = vs_layout

  (* Virtual shape constructors *)

  let runtime t =
    let vs_desc = Runtime t in
    let vs_layout =
      Layout.Base
        (Runtime_layout.to_base_layout (Runtime_shape.to_runtime_layout t))
    in
    { vs_desc;
      vs_layout;
      vs_hash = Hashtbl.hash (hash_runtime, Runtime_shape.hash t)
    }

  let rec unboxed_tuple args =
    let vs_desc =
      UnboxedProduct
        { unboxed_product_components = args;
          unboxed_product_kind = Unboxed_tuple
        }
    in
    { vs_desc;
      vs_layout = Layout.Product (List.map to_layout args);
      vs_hash =
        Hashtbl.hash
          ( hash_unboxed_product,
            hash_unboxed_product_kind Unboxed_tuple,
            List.map hash args )
    }

  and record_unboxed args =
    let components = List.map snd args in
    let field_names = List.map fst args in
    let vs_desc =
      UnboxedProduct
        { unboxed_product_components = components;
          unboxed_product_kind = Unboxed_record field_names
        }
    in
    { vs_desc;
      vs_layout = Layout.Product (List.map to_layout components);
      vs_hash =
        Hashtbl.hash
          ( hash_unboxed_product,
            hash_unboxed_product_kind (Unboxed_record field_names),
            List.map hash components )
    }

  let void =
    let vs_desc = Void in
    { vs_desc;
      vs_layout = Layout.Base Sort.Void;
      vs_hash = Hashtbl.hash hash_void
    }

  let rec print fmt { vs_desc } =
    match vs_desc with
    | Runtime s -> Runtime_shape.print fmt s
    | Void -> Format.fprintf fmt "void"
    | UnboxedProduct { unboxed_product_components; unboxed_product_kind } -> (
      match unboxed_product_kind with
      | Unboxed_record field_names ->
        Format.fprintf fmt "#{ %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (shape, name) ->
               Format.fprintf fmt "%s: %a" name print shape))
          (List.combine unboxed_product_components field_names)
      | Unboxed_tuple ->
        Format.fprintf fmt "#(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
             print)
          unboxed_product_components)

  let equal_unboxed_product_kind kind1 kind2 =
    match kind1, kind2 with
    | Unboxed_record names1, Unboxed_record names2 ->
      List.equal String.equal names1 names2
    | Unboxed_tuple, Unboxed_tuple -> true
    | (Unboxed_record _ | Unboxed_tuple), _ -> false

  let rec equal { vs_desc = desc1 } { vs_desc = desc2 } =
    match desc1, desc2 with
    | Runtime s1, Runtime s2 -> Runtime_shape.equal s1 s2
    | Void, Void -> true
    | ( UnboxedProduct
          { unboxed_product_kind = kind1; unboxed_product_components = comps1 },
        UnboxedProduct
          { unboxed_product_kind = kind2; unboxed_product_components = comps2 }
      ) ->
      equal_unboxed_product_kind kind1 kind2 && List.equal equal comps1 comps2
    | (Runtime _ | Void | UnboxedProduct _), _ -> false
end

(* Translation functions *)

exception Layout_missing

let translate_simd_vec_split :
    S.Predef.simd_vec_split -> Runtime_shape.simd_vec_split = function
  | Int8x16 -> Int8x16
  | Int16x8 -> Int16x8
  | Int32x4 -> Int32x4
  | Int64x2 -> Int64x2
  | Float32x4 -> Float32x4
  | Float64x2 -> Float64x2
  | Int8x32 -> Int8x32
  | Int16x16 -> Int16x16
  | Int32x8 -> Int32x8
  | Int64x4 -> Int64x4
  | Float32x8 -> Float32x8
  | Float64x4 -> Float64x4
  | Int8x64 -> Int8x64
  | Int16x32 -> Int16x32
  | Int32x16 -> Int32x16
  | Int64x8 -> Int64x8
  | Float32x16 -> Float32x16
  | Float64x8 -> Float64x8

let translate_unboxed : S.Predef.unboxed -> Runtime_shape.unboxed = function
  | Unboxed_float -> Unboxed_float
  | Unboxed_float32 -> Unboxed_float32
  | Unboxed_nativeint -> Unboxed_nativeint
  | Unboxed_int64 -> Unboxed_int64
  | Unboxed_int32 -> Unboxed_int32
  | Unboxed_int16 -> Unboxed_int16
  | Unboxed_int8 -> Unboxed_int8
  | Unboxed_simd svs -> Unboxed_simd (translate_simd_vec_split svs)

let err_exn f =
  if !Clflags.dwarf_pedantic then f Misc.fatal_errorf else raise Layout_missing
(* There are various error cases that we want to detect in pedantic mode, but
   where we want to recover gracefully otherwise. *)

(* CR mshinwell: it seems like this should move to the frontend *)
let rec layout_to_types_layout (ly : Layout.t) : Types.mixed_block_element =
  match ly with
  | Base base -> (
    match base with
    | Value -> Value
    | Float64 -> Float64
    (* This is a case, where we potentially have mapped [Float_boxed] to
       [Float64], but that is fine, because they are reordered like other mixed
       fields. *)
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
    | Void -> Product [||])
  | Product lys -> Product (Array.of_list (List.map layout_to_types_layout lys))

let to_runtime_layout (e : _ Mixed_block_shape.Singleton_mixed_block_element.t)
    : Runtime_layout.t =
  match e with
  | Value _ -> Value
  | Float_boxed _ -> Float64 (* unboxed in the actual block *)
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

(* In a mixed block, we want to add names for record fields but not for tuple
   fields. To treat them uniformly during unarization, we accumulate the record
   labels and tuple indices and then let the caller decide whether they want to
   use the resulting access name or not. *)
let projection_component_name (i : int) (field_name : string option) : string =
  match field_name with Some n -> n | None -> string_of_int (i + 1)

let runtime_shape (vs : Virtual_shape.t) =
  match vs.vs_desc with Runtime s -> Some s | Void | UnboxedProduct _ -> None

let force_runtime_shape_exn pt =
  match runtime_shape pt with Some s -> s | None -> raise Layout_missing

let rec project_field_given_path_exn
    (fields : (string option * Virtual_shape.t) array) (name_acc : string list)
    path =
  match path with
  | [] ->
    err_exn (fun f ->
        f "project_field_given_path_exn: argument cannot be empty.")
    (* This case cannot happen recursively. [lay_out_into_mixed_block_exn]
       currently ensures that it is ruled out with a more informative error. *)
  | i :: _ when i < 0 || i >= Array.length fields ->
    err_exn (fun f ->
        f
          "project_field_given_path_exn: invalid path, index %d out of bounds \
           0..<%d. Fields are: %a and accumulated name is %a."
          i (Array.length fields)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (name_opt, vs) ->
               match name_opt with
               | None -> Virtual_shape.print fmt vs
               | Some name ->
                 Format.fprintf fmt "%s: %a" name Virtual_shape.print vs))
          (Array.to_list fields)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
             Format.pp_print_string)
          (List.rev name_acc))
  | [i] ->
    let bf_name, bf_type = Array.get fields i in
    bf_type, List.rev (projection_component_name i bf_name :: name_acc)
  | i :: subpath ->
    let bf_name, bf_type = Array.get fields i in
    let inner_fields = flatten_product_layout_exn bf_type in
    let name_acc = projection_component_name i bf_name :: name_acc in
    project_field_given_path_exn (Array.of_list inner_fields) name_acc subpath

and flatten_product_layout_exn (vs : Virtual_shape.t) =
  match vs.vs_desc with
  | Void ->
    err_exn (fun f -> f "flatten_product_layout_exn: cannot project from Void.")
    (* All runtime type formers are register size and cannot be projected
       from. *)
  | Runtime t ->
    err_exn (fun f ->
        f
          "flatten_product_layout_exn: found unexpected runtime shape when \
           projecting: %a"
          Runtime_shape.print t)
  | UnboxedProduct
      { unboxed_product_components;
        unboxed_product_kind = Unboxed_record field_names
      } ->
    let fields = List.combine field_names unboxed_product_components in
    List.map (fun (field_name, arg) -> Some field_name, arg) fields
  | UnboxedProduct
      { unboxed_product_components; unboxed_product_kind = Unboxed_tuple } ->
    List.map (fun arg -> None, arg) unboxed_product_components

(* Raises if we hit a void field unintentionally. *)
let lay_out_into_mixed_block_exn
    ~(source_level_fields : (string option * Virtual_shape.t) list) =
  let layouts =
    List.map
      (fun (bf_name, bf_type) -> Virtual_shape.to_layout bf_type)
      source_level_fields
  in
  let source_level_fields_array = Array.of_list source_level_fields in
  let mixed_block_shapes =
    List.map (fun layout -> layout_to_types_layout layout) layouts
  in
  let reordering =
    Mixed_block_shape.of_mixed_block_elements
      ~print_locality:(fun _ _ -> ())
      (Lambda.transl_mixed_product_shape (Array.of_list mixed_block_shapes))
  in
  let field_layouts = Mixed_block_shape.flattened_reordered_shape reordering in
  let fields =
    Array.init (Mixed_block_shape.new_block_length reordering) (fun i ->
        let old_path = Mixed_block_shape.new_index_to_old_path reordering i in
        if List.length old_path = 0
        then
          err_exn (fun f ->
              let pp =
                Format.pp_print_list ~pp_sep:Format.pp_print_space
                  Virtual_shape.print
              in
              let source_level_fields =
                List.map (fun (_, bf_type) -> bf_type) source_level_fields
              in
              f "layout_mixed_block: empty path encountered when layouting %a"
                pp source_level_fields);
        let virtual_shape, label_components =
          project_field_given_path_exn source_level_fields_array [] old_path
        in
        (* After projection, we should not have a void field. The new indices
           should only point to non-void fields of the record. *)
        let shape =
          match runtime_shape virtual_shape with
          | Some s -> s
          | None ->
            err_exn (fun f ->
                f
                  "Found virtual shape %a when projecting runtime shapes from \
                   mixed block."
                  Virtual_shape.print virtual_shape)
        in
        let base_layout = to_runtime_layout field_layouts.(i) in
        let label = String.concat ".#" label_components in
        Runtime_shape.
          { mbf_type = shape; mbf_element = base_layout; mbf_label = label })
  in
  Array.to_list fields

let rec flatten_virtual_shape (vs : Virtual_shape.t) :
    Runtime_shape.t or_void list =
  match vs.vs_desc with
  | Void -> [Void]
  | Runtime s -> [Other s]
  | UnboxedProduct { unboxed_product_components; unboxed_product_kind = _ } ->
    List.concat_map flatten_virtual_shape unboxed_product_components

let erase_void (rs : 'a or_void list) : 'a list =
  List.filter_map (function Other s -> Some s | Void -> None) rs

(** Lays out the elements in the shape sequentailly while erasing all voids.
    This can be used for turning a virtual shape into a sequence of runtime
    shapes for arrays.  *)
let lay_out_sequentially (vs : Virtual_shape.t) : Runtime_shape.t list =
  (* CR sspies: Maybe bring back the names here for records? *)
  flatten_virtual_shape vs |> erase_void

let rec layout_to_unknown_shape (ly : Layout.t) : Virtual_shape.t =
  match ly with
  | Product lys ->
    let layouts = List.map layout_to_unknown_shape lys in
    Virtual_shape.unboxed_tuple layouts
  | Base b -> (
    match Runtime_layout.of_base_layout b with
    | Other runtime_layout ->
      Virtual_shape.runtime (Runtime_shape.unknown runtime_layout)
    | Void -> Virtual_shape.void)

module Shape_cache : sig
  val find_in_cache :
    Layout.t -> Shape.t -> rec_env:'a S.DeBruijn_env.t -> Virtual_shape.t option

  val add_to_cache :
    Shape.t ->
    Layout.t ->
    Virtual_shape.t ->
    rec_env:'a S.DeBruijn_env.t ->
    unit
end = struct
  type t =
    { type_shape : Shape.t;
      type_layout : Layout.t
    }

  module Cache = Hashtbl.Make (struct
    type nonrec t = t

    let equal ({ type_shape = x1; type_layout = y1 } : t)
        ({ type_shape = x2; type_layout = y2 } : t) =
      Shape.equal x1 x2 && Layout.equal y1 y2

    let hash { type_shape; type_layout } =
      Hashtbl.hash (type_shape.hash, type_layout)
    (* CR sspies: Add a hash function to Layout.t *)
  end)

  let cache = Cache.create 100

  let find_in_cache type_layout type_shape ~rec_env =
    if S.DeBruijn_env.is_empty rec_env
    then Cache.find_opt cache { type_shape; type_layout }
    else None

  let add_to_cache type_shape type_layout value ~rec_env =
    (* [rec_env] being empty means that the shape is closed. *)
    if S.DeBruijn_env.is_empty rec_env
    then Cache.add cache { type_shape; type_layout } value
end

let rec type_shape_to_dwarf_shape_exn ~rec_env (type_shape : Shape.t)
    (type_layout : Layout.t option) : Virtual_shape.t =
  let unknown_shape =
    match type_layout with
    | Some type_layout -> fun () -> layout_to_unknown_shape type_layout
    | None -> fun () -> raise Layout_missing
  in
  let err f =
    if !Clflags.dwarf_pedantic then f Misc.fatal_errorf else unknown_shape ()
  in
  match type_shape.desc, type_layout with
  | Leaf, _ -> unknown_shape ()
  | Unknown_type, _ -> unknown_shape ()
  | Tuple args, (None | Some (Base Value)) -> (
    let args =
      List.map
        (fun sh -> type_shape_to_dwarf_shape ~rec_env sh (Layout.Base Value))
        (* CR sspies: In the future, we cannot assume that these are always
           values. This means we may need to use [type_shape_to_dwarf_shape_exn]
           and catch the exception on the outside. *)
        args
    in
    try
      let args = List.map force_runtime_shape_exn args in
      (* CR sspies: In the future, we will need to use the mixed block logic
         here. *)
      Virtual_shape.runtime (Runtime_shape.tuple args)
    with Layout_missing -> Virtual_shape.runtime (Runtime_shape.unknown Value))
  | Tuple _, Some type_layout ->
    err (fun f ->
        f "tuple must have value layout, but got: %a" Layout.format type_layout)
  | At_layout (shape, layout), None ->
    type_shape_to_dwarf_shape ~rec_env shape layout
  | At_layout (shape, layout), Some type_layout
    when Layout.equal layout type_layout ->
    type_shape_to_dwarf_shape ~rec_env shape layout
  | At_layout (shape, layout), Some type_layout ->
    err (fun f ->
        f
          "shape at layout does not match expected layout, expected %a, got \
           %a; shape: %a"
          Layout.format type_layout Layout.format layout Shape.print shape)
  | Predef (pre, args), _ -> (
    try
      Virtual_shape.runtime
        (Runtime_shape.predef (predef_to_dwarf_shape_exn ~rec_env pre ~args))
    with Layout_missing -> (
      let layout = Shape.Predef.to_base_layout pre in
      let runtime_layout = Runtime_layout.of_base_layout layout in
      match runtime_layout with
      | Void -> Virtual_shape.void
      | Other runtime_layout ->
        Virtual_shape.runtime (Runtime_shape.unknown runtime_layout)))
  | Mu sh, type_layout ->
    (* We currently do not support unboxed recursive types (e.g., recursively
       defined mixed records). Those will fall back to default for printing the
       layout by forcing the runtime shape below. *)
    (* CR sspies: We should guess the layout from the recursive body instead of
       just using the current layout. *)
    let rec_env = Shape.DeBruijn_env.push rec_env type_layout in
    type_shape_to_dwarf_shape_exn ~rec_env sh type_layout
    |> force_runtime_shape_exn |> Runtime_shape.mu |> Virtual_shape.runtime
  | Rec_var i, layout -> (
    match Shape.DeBruijn_env.get_opt rec_env ~de_bruijn_index:i, layout with
    | Some (Some (Layout.Base base as ly1)), ly2_opt
      when Option.value ~default:true
             (Option.map (fun ly2 -> Layout.equal ly1 ly2) ly2_opt) -> (
      match Runtime_layout.of_base_layout base with
      | Void -> Virtual_shape.void
      | Other runtime_layout ->
        Virtual_shape.runtime (Runtime_shape.rec_var i runtime_layout))
    | Some (Some ly1), Some ly2 when not (Layout.equal ly1 ly2) ->
      err (fun f ->
          f "Recursive variable has wrong layout. Expected %a, got %a"
            Layout.format ly2 Layout.format ly1)
      (* In all cases below, if there are two layouts, they are the same. *)
    | (Some (Some ly1), (None | Some _)) as _ly2_opt ->
      (*= In this case, either:
          - [ly1] is a product and [_ly2_opt] is None
          - [ly1] is a product and [_ly2_opt] is equal to Some [ly1].

          If [ly1] is a base layout, we would have taken the first branch (if
          equal to the second layout) or the second branch (of not equal to the
          second layout). *)
      layout_to_unknown_shape ly1
    | (Some None | None), Some (Layout.Base base) -> (
      match Runtime_layout.of_base_layout base with
      | Void -> Virtual_shape.void
      | Other runtime_layout ->
        Virtual_shape.runtime (Runtime_shape.rec_var i runtime_layout))
    | (Some None | None), Some (Layout.Product _ as ly) ->
      layout_to_unknown_shape ly
    | (Some None | None), None -> raise Layout_missing)
  | Alias sh, type_layout ->
    type_shape_to_dwarf_shape_exn ~rec_env sh type_layout
  | Arrow, (None | Some (Base Value)) ->
    Virtual_shape.runtime Runtime_shape.func
  | Arrow, Some type_layout ->
    err (fun f ->
        f "function must have value layout, but got: %a" Layout.format
          type_layout)
  | Unboxed_tuple shapes, None ->
    Virtual_shape.unboxed_tuple
      (List.map
         (fun sh -> type_shape_to_dwarf_shape_exn ~rec_env sh None)
         shapes)
  | Unboxed_tuple fields, Some (Product lys)
    when List.length lys = List.length fields ->
    Virtual_shape.unboxed_tuple
      (List.map2 (type_shape_to_dwarf_shape ~rec_env) fields lys)
  | Unboxed_tuple _, Some (Base b) ->
    err (fun f ->
        f "unboxed tuple must have product layout, but got: %a" Layout.format
          (Layout.Base b))
  | Unboxed_tuple _, Some type_layout ->
    err (fun f ->
        f "unboxed tuple must have product layout, but got: %a" Layout.format
          type_layout)
  | Variant constructors, (None | Some (Base Value)) -> (
    try
      let constructors =
        List.map
          (fun { S.name; kind = _; args; constr_uid = _ } ->
            let is_tuple_constructor =
              List.for_all
                (fun { S.field_name; _ } -> Option.is_none field_name)
                args
            in
            let source_level_fields =
              List.map
                (fun { S.field_name;
                       S.field_uid = _;
                       S.field_value = arg, layout
                     } ->
                  field_name, type_shape_to_dwarf_shape ~rec_env arg layout)
                args
            in
            let constr_args =
              lay_out_into_mixed_block_exn ~source_level_fields
            in
            if is_tuple_constructor
            then
              (* We clear the field names for tuple constructors. *)
              Runtime_shape.Tuple_constructor
                { constr_name = name;
                  constr_args =
                    List.map
                      (fun ({ mbf_type; mbf_element; mbf_label = _ } :
                             string Runtime_shape.mixed_block_field) ->
                        Runtime_shape.{ mbf_type; mbf_element; mbf_label = () })
                      constr_args
                }
            else
              Runtime_shape.Record_constructor
                { constr_name = name; constr_args })
          constructors
      in
      Virtual_shape.runtime (Runtime_shape.variant constructors)
    with Layout_missing ->
      Virtual_shape.runtime (Runtime_shape.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] *))
  | Variant _, Some ((Product _ | Base _) as ly) ->
    err (fun f ->
        f "variant must have value layout, but got: %a" Layout.format ly)
  | Poly_variant constructors, (None | Some (Base Value)) -> (
    try
      let constructors =
        List.map
          (fun { S.pv_constr_name; pv_constr_args } ->
            let source_level_fields =
              List.map
                (fun arg ->
                  ( None,
                    type_shape_to_dwarf_shape ~rec_env arg (Layout.Base Value) ))
                pv_constr_args
            in
            let constr_args =
              lay_out_into_mixed_block_exn ~source_level_fields
            in
            Runtime_shape.Tuple_constructor
              { constr_name = pv_constr_name;
                constr_args =
                  List.map
                    (fun ({ mbf_type; mbf_element; mbf_label = _ } :
                           string Runtime_shape.mixed_block_field) ->
                      Runtime_shape.{ mbf_type; mbf_element; mbf_label = () })
                    constr_args
              })
          constructors
      in
      Virtual_shape.runtime (Runtime_shape.polymorphic_variant constructors)
    with Layout_missing ->
      Virtual_shape.runtime (Runtime_shape.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] *))
  | Poly_variant _, Some ((Product _ | Base _) as ly) ->
    err (fun f ->
        f "polymorphic variant must have value layout, but got: %a"
          Layout.format ly)
  | Record { fields; kind = Record_unboxed_product }, None ->
    Virtual_shape.record_unboxed
      (List.map
         (fun (field_name, _, field_value, field_layout) ->
           ( field_name,
             type_shape_to_dwarf_shape ~rec_env field_value field_layout ))
         fields)
  | Record { fields; kind = Record_unboxed_product }, Some (Product lys)
    when List.equal Layout.equal (List.map (fun (_, _, _, ly) -> ly) fields) lys
    ->
    Virtual_shape.record_unboxed
      (List.map
         (fun (field_name, _, field_value, field_layout) ->
           ( field_name,
             type_shape_to_dwarf_shape ~rec_env field_value field_layout ))
         fields)
  | ( Record { fields; kind = Record_unboxed_product },
      Some ((Base _ | Product _) as ly) ) ->
    let layout_from_shapes =
      Layout.Product (List.map (fun (_, _, _, ly) -> ly) fields)
    in
    err (fun f ->
        f "unboxed record expected to be of layout %a, but got: %a"
          Layout.format layout_from_shapes Layout.format ly)
  | Record { fields; kind = Record_boxed }, (None | Some (Base Value))
  | Record { fields; kind = Record_floats }, (None | Some (Base Value))
  | Record { fields; kind = Record_mixed _ }, (None | Some (Base Value)) -> (
    try
      let source_level_fields =
        List.map
          (fun (field_name, _, field_type, field_layout) ->
            let shape =
              type_shape_to_dwarf_shape ~rec_env field_type field_layout
            in
            Some field_name, shape)
          fields
      in
      let fields = lay_out_into_mixed_block_exn ~source_level_fields in
      Virtual_shape.runtime (Runtime_shape.record_mixed fields)
    with Layout_missing ->
      Virtual_shape.runtime (Runtime_shape.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] at the
         moment *)
      (* CR sspies: In the future, if the inner layouts become options, we need
         to use [type_shape_to_dwarf_shape_exn] here and catch the error. *))
  | ( Record { fields; kind = Record_boxed | Record_mixed _ | Record_floats },
      Some ((Product _ | Base _) as ly) ) ->
    err (fun f ->
        f "record must have value layout, but got: %a" Layout.format ly)
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      layout )
    when Option.value ~default:true
           (Option.map (fun ly -> Layout.equal ly field_layout) layout) -> (
    let shape = type_shape_to_dwarf_shape ~rec_env field_type field_layout in
    match lay_out_sequentially shape with
    | [shape] ->
      Virtual_shape.runtime
        (Runtime_shape.record_attribute_unboxed
           ~contents:
             Runtime_shape.
               { mbf_type = shape;
                 mbf_element = Runtime_shape.to_runtime_layout shape;
                 mbf_label = field_name
               })
    | _ -> shape
    (* We drop [@@unboxed] records/variants when they contain actual product
       types in the debugger. *))
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      None ) ->
    assert false (* ruled out by the [Option.value ~default:true] above. *)
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      Some ly ) ->
    err (fun f ->
        f
          "record with [@@unboxed] attribute with expected layout %a, but \
           field has layout %a"
          Layout.format ly Layout.format field_layout)
  | Record { fields = ([] | _ :: _ :: _) as fields; kind = Record_unboxed }, _
    ->
    err (fun f ->
        f
          "record with [@@unboxed] attribute must have exactly one field, but \
           has: { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (name, _, shape, _) ->
               Format.fprintf fmt "%s: %a;" name S.print shape))
          fields)
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      layout )
    when Option.value ~default:true
           (Option.map (fun ly -> Layout.equal ly arg_layout) layout) -> (
    let shape = type_shape_to_dwarf_shape ~rec_env arg_shape arg_layout in
    match lay_out_sequentially shape with
    | [shape] ->
      Virtual_shape.runtime
        (Runtime_shape.variant_attribute_unboxed ~constructor_name:name
           ~constructor_arg:
             Runtime_shape.
               { mbf_type = shape;
                 mbf_element = Runtime_shape.to_runtime_layout shape;
                 mbf_label = arg_name
               })
    | _ -> shape
    (* We drop [@@unboxed] records/variants when they contain actual product
       types in the debugger. *))
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      None ) ->
    assert false (* ruled out by the [Option.value ~default:true] above. *)
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      Some ly ) ->
    err (fun f ->
        f
          "variant with [@@unboxed] attribute with expected layout %a, but \
           field has layout %a"
          Layout.format ly Layout.format arg_layout)
  | ( ( Var _ | Error _ | Proj _ | Abs _ | Comp_unit _ | Struct _ | Mutrec _
      | Constr _ | App _ | Proj_decl _ ),
      _ ) ->
    unknown_shape ()

and predef_to_dwarf_shape_exn ~rec_env (predef : S.Predef.t) ~args :
    Runtime_shape.predef =
  match predef, args with
  | Array, [elem_shape] -> (
    let elem_shape = type_shape_to_dwarf_shape_exn ~rec_env elem_shape None in
    let children = lay_out_sequentially elem_shape in
    match children with
    | [] -> err_exn (fun f -> f "array cannot contain only void elements")
    | [child] -> Runtime_shape.Array_regular child
    | children -> Runtime_shape.Array_packed children
    (* case for an unboxed product inside *))
  | Bytes, [] -> Runtime_shape.Bytes
  | Char, [] -> Runtime_shape.Char
  | Extension_constructor, [] -> Runtime_shape.Extension_constructor
  | Float, [] -> Runtime_shape.Float
  | Float32, [] -> Runtime_shape.Float32
  | Floatarray, [] -> Runtime_shape.Floatarray
  | Int, [] -> Runtime_shape.Int
  | Int8, [] -> Runtime_shape.Int8
  | Int16, [] -> Runtime_shape.Int16
  | Int32, [] -> Runtime_shape.Int32
  | Int64, [] -> Runtime_shape.Int64
  | Lazy_t, [elem_shape] ->
    let elem_shape =
      type_shape_to_dwarf_shape_exn ~rec_env elem_shape
        (Some (Layout.Base Value))
    in
    let elem_shape =
      match runtime_shape elem_shape with
      | Some s -> s
      | None -> Runtime_shape.unknown Value
    in
    Runtime_shape.Lazy_t elem_shape
  | Nativeint, [] -> Runtime_shape.Nativeint
  | String, [] -> Runtime_shape.String
  | Simd vec_split, [] ->
    Runtime_shape.Simd (translate_simd_vec_split vec_split)
  | Exception, [] -> Runtime_shape.Exception
  | Unboxed unb, [] -> Runtime_shape.Unboxed (translate_unboxed unb)
  | ( ( Bytes | Char | Extension_constructor | Float | Float32 | Floatarray
      | Int | Int8 | Int16 | Int32 | Int64 | Nativeint | String | Simd _
      | Exception | Unboxed _ ),
      arg :: args ) ->
    err_exn (fun f ->
        f "predefined type %a does not take arguments, but got arguments %a"
          S.Predef.print predef
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          (arg :: args))
  | Array, ([] | _ :: _ :: _) ->
    err_exn (fun f ->
        f
          "predefined type array requires exactly one argument, but got %d \
           arguments: %a"
          (List.length args)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          args)
  | Lazy_t, ([] | _ :: _ :: _) ->
    err_exn (fun f ->
        f
          "predefined type lazy_t requires exactly one argument, but got %d \
           arguments: %a"
          (List.length args)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          args)

and type_shape_to_dwarf_shape ~rec_env type_shape type_layout : Virtual_shape.t
    =
  match Shape_cache.find_in_cache type_layout type_shape ~rec_env with
  | Some shape -> shape
  | None ->
    let shape =
      try type_shape_to_dwarf_shape_exn ~rec_env type_shape (Some type_layout)
      with Layout_missing -> layout_to_unknown_shape type_layout
    in
    Shape_cache.add_to_cache type_shape type_layout shape ~rec_env;
    shape

let type_shape_to_dwarf_shape type_shape type_layout =
  type_shape_to_dwarf_shape ~rec_env:Shape.DeBruijn_env.empty type_shape
    type_layout
