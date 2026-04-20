(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype
module Jkind = Btype.Jkind0

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

(* Note: [ident_create] creates identifiers with [Ident.Predef], and later
   portions of the compiler assume that expressions with these identifiers must
   have types with layout value (see, e.g., the compilation of [Pgetpredef]
   in `lambda.ml`). *)
let ident_create = wrap Ident.create_predef

type abstract_type_constr = [
  | `Int
  | `Char
  | `String
  | `Bytes
  | `Float
  | `Continuation
  | `Array
  | `Nativeint
  | `Int32
  | `Int64
  | `Lazy_t
  | `Extension_constructor
  | `Floatarray
  | `Iarray
  | `Atomic_loc
  | `Lexing_position
  | `Code
  | `Float32
  | `Int8
  | `Int16
]
type abstract_non_value_type_constr = [
  | `Idx_imm
  | `Idx_mut
  | `Int8x16
  | `Int16x8
  | `Int32x4
  | `Int64x2
  | `Float16x8
  | `Float32x4
  | `Float64x2
  | `Int8x32
  | `Int16x16
  | `Int32x8
  | `Int64x4
  | `Float16x16
  | `Float32x8
  | `Float64x4
  | `Int8x64
  | `Int16x32
  | `Int32x16
  | `Int64x8
  | `Float16x32
  | `Float32x16
  | `Float64x8
]
type data_type_constr = [
  | `Bool
  | `Unit
  | `Exn
  | `Eff
  | `List
  | `Option
  | `Or_null
]
type type_constr = [
  | abstract_type_constr
  | abstract_non_value_type_constr
  | data_type_constr
]

let non_extension_type_constrs : type_constr list = [
  `Int;
  `Char;
  `String;
  `Bytes;
  `Float;
  `Bool;
  `Unit;
  `Exn;
  `Eff;
  `Continuation;
  `Array;
  `List;
  `Option;
  `Nativeint;
  `Int32;
  `Int64;
  `Lazy_t;
  `Extension_constructor;
  `Floatarray;
  `Iarray;
  `Atomic_loc;
  `Lexing_position;
  `Code;
  `Idx_imm;
  `Idx_mut;
  `Or_null;
]

let simd_stable_extension_type_constrs : type_constr list = [
  `Int8x16;
  `Int16x8;
  `Int32x4;
  `Int64x2;
  `Float16x8;
  `Float32x4;
  `Float64x2;
  `Int8x32;
  `Int16x16;
  `Int32x8;
  `Int64x4;
  `Float16x16;
  `Float32x8;
  `Float64x4;
]

let simd_beta_extension_type_constrs : type_constr list = []

let simd_alpha_extension_type_constrs : type_constr list = [
  `Int8x64;
  `Int16x32;
  `Int32x16;
  `Int64x8;
  `Float16x32;
  `Float32x16;
  `Float64x8;
]

let small_number_extension_type_constrs : type_constr list = [
  `Float32;
  `Int8;
  `Int16;
]

let all_type_constrs = (
  non_extension_type_constrs
  @ simd_stable_extension_type_constrs
  @ simd_beta_extension_type_constrs
  @ simd_alpha_extension_type_constrs
)

let ident_int = ident_create "int"
and ident_char = ident_create "char"
and ident_bytes = ident_create "bytes"
and ident_float = ident_create "float"
and ident_float32 = ident_create "float32"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_exn = ident_create "exn"
and ident_eff = ident_create "eff"
and ident_continuation = ident_create "continuation"
and ident_array = ident_create "array"
and ident_list = ident_create "list"
and ident_option = ident_create "option"
and ident_nativeint = ident_create "nativeint"
and ident_int8 = ident_create "int8"
and ident_int16 = ident_create "int16"
and ident_int32 = ident_create "int32"
and ident_int64 = ident_create "int64"
and ident_lazy_t = ident_create "lazy_t"
and ident_string = ident_create "string"
and ident_extension_constructor = ident_create "extension_constructor"
and ident_floatarray = ident_create "floatarray"
and ident_iarray = ident_create "iarray"
and ident_atomic_loc = ident_create "atomic_loc"
and ident_lexing_position = ident_create "lexing_position"
(* CR metaprogramming aivaskovic: there is a question about naming;
   keep `expr` for now instead of `code` *)
and ident_code = ident_create "expr"

and ident_or_null = ident_create "or_null"
and ident_idx_imm = ident_create "idx_imm"
and ident_idx_mut = ident_create "idx_mut"

and ident_int8x16 = ident_create "int8x16"
and ident_int16x8 = ident_create "int16x8"
and ident_int32x4 = ident_create "int32x4"
and ident_int64x2 = ident_create "int64x2"
and ident_float16x8 = ident_create "float16x8"
and ident_float32x4 = ident_create "float32x4"
and ident_float64x2 = ident_create "float64x2"
and ident_int8x32 = ident_create "int8x32"
and ident_int16x16 = ident_create "int16x16"
and ident_int32x8 = ident_create "int32x8"
and ident_int64x4 = ident_create "int64x4"
and ident_float16x16 = ident_create "float16x16"
and ident_float32x8 = ident_create "float32x8"
and ident_float64x4 = ident_create "float64x4"
and ident_int8x64 = ident_create "int8x64"
and ident_int16x32 = ident_create "int16x32"
and ident_int32x16 = ident_create "int32x16"
and ident_int64x8 = ident_create "int64x8"
and ident_float16x32 = ident_create "float16x32"
and ident_float32x16 = ident_create "float32x16"
and ident_float64x8 = ident_create "float64x8"

let ident_of_type_constr : type_constr -> Ident.t = function
  | `Int -> ident_int
  | `Char -> ident_char
  | `String -> ident_string
  | `Bytes -> ident_bytes
  | `Float -> ident_float
  | `Bool -> ident_bool
  | `Unit -> ident_unit
  | `Exn -> ident_exn
  | `Eff -> ident_eff
  | `Continuation -> ident_continuation
  | `Array -> ident_array
  | `List -> ident_list
  | `Option -> ident_option
  | `Nativeint -> ident_nativeint
  | `Int32 -> ident_int32
  | `Int64 -> ident_int64
  | `Lazy_t -> ident_lazy_t
  | `Extension_constructor -> ident_extension_constructor
  | `Floatarray -> ident_floatarray
  | `Iarray -> ident_iarray
  | `Atomic_loc -> ident_atomic_loc
  | `Lexing_position -> ident_lexing_position
  | `Code -> ident_code
  | `Float32 -> ident_float32
  | `Int8 -> ident_int8
  | `Int16 -> ident_int16
  | `Idx_imm -> ident_idx_imm
  | `Idx_mut -> ident_idx_mut
  | `Int8x16 -> ident_int8x16
  | `Int16x8 -> ident_int16x8
  | `Int32x4 -> ident_int32x4
  | `Int64x2 -> ident_int64x2
  | `Float16x8 -> ident_float16x8
  | `Float32x4 -> ident_float32x4
  | `Float64x2 -> ident_float64x2
  | `Int8x32 -> ident_int8x32
  | `Int16x16 -> ident_int16x16
  | `Int32x8 -> ident_int32x8
  | `Int64x4 -> ident_int64x4
  | `Float16x16 -> ident_float16x16
  | `Float32x8 -> ident_float32x8
  | `Float64x4 -> ident_float64x4
  | `Int8x64 -> ident_int8x64
  | `Int16x32 -> ident_int16x32
  | `Int32x16 -> ident_int32x16
  | `Int64x8 -> ident_int64x8
  | `Float16x32 -> ident_float16x32
  | `Float32x16 -> ident_float32x16
  | `Float64x8 -> ident_float64x8
  | `Or_null -> ident_or_null

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
and path_float32 = Pident ident_float32
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_eff = Pident ident_eff
and path_continuation = Pident ident_continuation
and path_array = Pident ident_array
and path_list = Pident ident_list
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int8 = Pident ident_int8
and path_int16 = Pident ident_int16
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_lazy_t = Pident ident_lazy_t
and path_string = Pident ident_string
and path_extension_constructor = Pident ident_extension_constructor
and path_floatarray = Pident ident_floatarray
and path_iarray = Pident ident_iarray
and path_atomic_loc = Pident ident_atomic_loc
and path_lexing_position = Pident ident_lexing_position
and path_idx_imm = Pident ident_idx_imm
and path_idx_mut = Pident ident_idx_mut
and path_code = Pident ident_code

and path_or_null = Pident ident_or_null

and path_int8x16 = Pident ident_int8x16
and path_int16x8 = Pident ident_int16x8
and path_int32x4 = Pident ident_int32x4
and path_int64x2 = Pident ident_int64x2
and path_float16x8 = Pident ident_float16x8
and path_float32x4 = Pident ident_float32x4
and path_float64x2 = Pident ident_float64x2
and path_int8x32 = Pident ident_int8x32
and path_int16x16 = Pident ident_int16x16
and path_int32x8 = Pident ident_int32x8
and path_int64x4 = Pident ident_int64x4
and path_float16x16 = Pident ident_float16x16
and path_float32x8 = Pident ident_float32x8
and path_float64x4 = Pident ident_float64x4
and path_int8x64 = Pident ident_int8x64
and path_int16x32 = Pident ident_int16x32
and path_int32x16 = Pident ident_int32x16
and path_int64x8 = Pident ident_int64x8
and path_float16x32 = Pident ident_float16x32
and path_float32x16 = Pident ident_float32x16
and path_float64x8 = Pident ident_float64x8

let path_unboxed_float = Path.unboxed_version path_float
and path_unboxed_unit = Path.unboxed_version path_unit
and path_unboxed_bool = Path.unboxed_version path_bool
and path_unboxed_float32 = Path.unboxed_version path_float32
and path_unboxed_nativeint = Path.unboxed_version path_nativeint
and path_unboxed_char = Path.unboxed_version path_char
and path_unboxed_int = Path.unboxed_version path_int
and path_unboxed_int8 = Path.unboxed_version path_int8
and path_unboxed_int16 = Path.unboxed_version path_int16
and path_unboxed_int32 = Path.unboxed_version path_int32
and path_unboxed_int64 = Path.unboxed_version path_int64

and path_unboxed_int8x16 = Path.unboxed_version path_int8x16
and path_unboxed_int16x8 = Path.unboxed_version path_int16x8
and path_unboxed_int32x4 = Path.unboxed_version path_int32x4
and path_unboxed_int64x2 = Path.unboxed_version path_int64x2
and path_unboxed_float16x8 = Path.unboxed_version path_float16x8
and path_unboxed_float32x4 = Path.unboxed_version path_float32x4
and path_unboxed_float64x2 = Path.unboxed_version path_float64x2
and path_unboxed_int8x32 = Path.unboxed_version path_int8x32
and path_unboxed_int16x16 = Path.unboxed_version path_int16x16
and path_unboxed_int32x8 = Path.unboxed_version path_int32x8
and path_unboxed_int64x4 = Path.unboxed_version path_int64x4
and path_unboxed_float16x16 = Path.unboxed_version path_float16x16
and path_unboxed_float32x8 = Path.unboxed_version path_float32x8
and path_unboxed_float64x4 = Path.unboxed_version path_float64x4
and path_unboxed_int8x64 = Path.unboxed_version path_int8x64
and path_unboxed_int16x32 = Path.unboxed_version path_int16x32
and path_unboxed_int32x16 = Path.unboxed_version path_int32x16
and path_unboxed_int64x8 = Path.unboxed_version path_int64x8
and path_unboxed_float16x32 = Path.unboxed_version path_float16x32
and path_unboxed_float32x16 = Path.unboxed_version path_float32x16
and path_unboxed_float64x8 = Path.unboxed_version path_float64x8

let path_of_type_constr typ =
  Pident (ident_of_type_constr typ)

let tconstr p args = newgenty (Tconstr(p, args, ref Mnil))
let type_int = tconstr path_int []
and type_int8 = tconstr path_int8 []
and type_int16 = tconstr path_int16 []
and type_char = tconstr path_char []
and type_bytes = tconstr path_bytes []
and type_float = tconstr path_float []
and type_float32 = tconstr path_float32 []
and type_bool = tconstr path_bool []
and type_unit = tconstr path_unit []
and type_exn = tconstr path_exn []
and type_eff t = tconstr path_eff [t]
and type_continuation t1 t2 = tconstr path_continuation [t1; t2]
and type_array t = tconstr path_array [t]
and type_list t = tconstr path_list [t]
and type_option t = tconstr path_option [t]
and type_nativeint = tconstr path_nativeint []
and type_int32 = tconstr path_int32 []
and type_int64 = tconstr path_int64 []
and type_lazy_t t = tconstr path_lazy_t [t]
and type_string = tconstr path_string []
and type_extension_constructor = tconstr path_extension_constructor []
and type_floatarray = tconstr path_floatarray []
and type_iarray t = tconstr path_iarray [t]
and type_atomic_loc t = tconstr path_atomic_loc [t]
and type_lexing_position = tconstr path_lexing_position []
and type_code t = tconstr path_code [t]

and type_unboxed_unit = tconstr path_unboxed_unit []
and type_unboxed_bool = tconstr path_unboxed_bool []
and type_unboxed_float = tconstr path_unboxed_float []
and type_unboxed_float32 = tconstr path_unboxed_float32 []
and type_unboxed_nativeint = tconstr path_unboxed_nativeint []
and type_unboxed_int32 = tconstr path_unboxed_int32 []
and type_unboxed_int64 = tconstr path_unboxed_int64 []
and type_unboxed_char = tconstr path_unboxed_char []
and type_unboxed_int = tconstr path_unboxed_int []
and type_unboxed_int8 = tconstr path_unboxed_int8 []
and type_unboxed_int16 = tconstr path_unboxed_int16 []
and type_or_null t = tconstr path_or_null [t]
and type_idx_imm t1 t2 = tconstr path_idx_imm [t1; t2]
and type_idx_mut t1 t2 = tconstr path_idx_mut [t1; t2]

and type_int8x16 = tconstr path_int8x16 []
and type_int16x8 = tconstr path_int16x8 []
and type_int32x4 = tconstr path_int32x4 []
and type_int64x2 = tconstr path_int64x2 []
and type_float16x8 = tconstr path_float16x8 []
and type_float32x4 = tconstr path_float32x4 []
and type_float64x2 = tconstr path_float64x2 []
and type_int8x32 = tconstr path_int8x32 []
and type_int16x16 = tconstr path_int16x16 []
and type_int32x8 = tconstr path_int32x8 []
and type_int64x4 = tconstr path_int64x4 []
and type_float16x16 = tconstr path_float16x16 []
and type_float32x8 = tconstr path_float32x8 []
and type_float64x4 = tconstr path_float64x4 []
and type_int8x64 = tconstr path_int8x64 []
and type_int16x32 = tconstr path_int16x32 []
and type_int32x16 = tconstr path_int32x16 []
and type_int64x8 = tconstr path_int64x8 []
and type_float16x32 = tconstr path_float16x32 []
and type_float32x16 = tconstr path_float32x16 []
and type_float64x8 = tconstr path_float64x8 []

and type_unboxed_int8x16 = tconstr path_unboxed_int8x16 []
and type_unboxed_int16x8 = tconstr path_unboxed_int16x8 []
and type_unboxed_int32x4 = tconstr path_unboxed_int32x4 []
and type_unboxed_int64x2 = tconstr path_unboxed_int64x2 []
and type_unboxed_float16x8 = tconstr path_unboxed_float16x8 []
and type_unboxed_float32x4 = tconstr path_unboxed_float32x4 []
and type_unboxed_float64x2 = tconstr path_unboxed_float64x2 []
and type_unboxed_int8x32 = tconstr path_unboxed_int8x32 []
and type_unboxed_int16x16 = tconstr path_unboxed_int16x16 []
and type_unboxed_int32x8 = tconstr path_unboxed_int32x8 []
and type_unboxed_int64x4 = tconstr path_unboxed_int64x4 []
and type_unboxed_float16x16 = tconstr path_unboxed_float16x16 []
and type_unboxed_float32x8 = tconstr path_unboxed_float32x8 []
and type_unboxed_float64x4 = tconstr path_unboxed_float64x4 []
and type_unboxed_int8x64 = tconstr path_unboxed_int8x64 []
and type_unboxed_int16x32 = tconstr path_unboxed_int16x32 []
and type_unboxed_int32x16 = tconstr path_unboxed_int32x16 []
and type_unboxed_int64x8 = tconstr path_unboxed_int64x8 []
and type_unboxed_float16x32 = tconstr path_unboxed_float16x32 []
and type_unboxed_float32x16 = tconstr path_unboxed_float32x16 []
and type_unboxed_float64x8 = tconstr path_unboxed_float64x8 []

let find_type_constr =
  let all_predef_paths =
    all_type_constrs
    |> List.map (fun tconstr -> path_of_type_constr tconstr, tconstr)
    |> Path.Map.of_list
  in
  fun p -> Path.Map.find_opt p all_predef_paths

let ident_match_failure = ident_create "Match_failure"
and ident_out_of_memory = ident_create "Out_of_memory"
and ident_out_of_fibers = ident_create "Out_of_fibers"
and ident_invalid_argument = ident_create "Invalid_argument"
and ident_failure = ident_create "Failure"
and ident_not_found = ident_create "Not_found"
and ident_sys_error = ident_create "Sys_error"
and ident_end_of_file = ident_create "End_of_file"
and ident_division_by_zero = ident_create "Division_by_zero"
and ident_stack_overflow = ident_create "Stack_overflow"
and ident_sys_blocked_io = ident_create "Sys_blocked_io"
and ident_assert_failure = ident_create "Assert_failure"
and ident_undefined_recursive_module =
        ident_create "Undefined_recursive_module"
and ident_continuation_already_taken = ident_create "Continuation_already_taken"

let all_predef_exns = [
  ident_match_failure;
  ident_out_of_memory;
  ident_out_of_fibers;
  ident_invalid_argument;
  ident_failure;
  ident_not_found;
  ident_sys_error;
  ident_end_of_file;
  ident_division_by_zero;
  ident_stack_overflow;
  ident_sys_blocked_io;
  ident_assert_failure;
  ident_undefined_recursive_module;
  ident_continuation_already_taken;
]

let path_match_failure = Pident ident_match_failure
and path_invalid_argument = Pident ident_invalid_argument
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_void = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"
and ident_none = ident_create "None"
and ident_some = ident_create "Some"

and ident_null = ident_create "Null"
and ident_this = ident_create "This"

let option_argument_sort = Jkind_types.Sort.Const.value
let option_argument_jkind = Jkind.Builtin.value_or_null ~why:(
  Type_argument {parent_path = path_option; position = 1; arity = 1})

let unrestricted tvar ca_sort =
  {
    ca_type=tvar;
    ca_sort;
    ca_modalities=Mode.Modality.Const.id;
    ca_loc=Location.none
  }

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Location.none;
    cd_attributes = [];
    cd_uid = Uid.of_predef_id id;
  }

let list_jkind param =
  Jkind.Builtin.immutable_data ~why:Boxed_variant |>
  Jkind.add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr:param |>
  Jkind.mark_best

let list_sort = Jkind_types.Sort.Const.value
let list_argument_sort = Jkind_types.Sort.Const.value
let list_argument_jkind = Jkind.Builtin.value_or_null ~why:(
  Type_argument {parent_path = path_list; position = 1; arity = 1})

<<<<<<< HEAD
let or_null_argument_sort = Jkind_types.Sort.Const.value

let or_null_jkind param =
  Jkind.Const.Builtin.value_or_null_mod_everything
  |> Jkind.of_builtin ~why:(Primitive ident_or_null)
  |> Jkind.add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr:param
  |> Jkind.mark_best

let or_null_kind tvar =
  let cstrs =
    [ cstr ident_null [];
      cstr ident_this [unrestricted tvar or_null_argument_sort]] in
  Type_variant (cstrs, Variant_with_null, None)

let decl_of_type_constr tconstr =
  let type_ident = ident_of_type_constr tconstr in
  let type_uid = Uid.of_predef_id type_ident in
  let decl0
      ?(kind = Type_abstract Definition)
      ~(jkind : jkind_l)
      ?(unboxed_jkind : Jkind.Const.Builtin.t option)
      ()
    =
||||||| f8c6716f8c
let mk_add_type add_type =
  let add_type_with_jkind
      ?manifest type_ident
      ?(kind=Type_abstract Definition)
      ~jkind
      ?unboxed_jkind
      env =
    let type_uid = Uid.of_predef_id type_ident in
=======
let predef_jkinds =
  List.map
    (fun (builtin : Jkind.Const.Builtin.t) ->
       ident_create builtin.name, builtin.jkind)
    Jkind.Const.Builtin.builtins

let all_predef_jkinds = List.map fst predef_jkinds

let add_predef_jkinds add_jkind env =
  List.fold_left
    (fun env (id, jkind) -> add_jkind id jkind env) env
    predef_jkinds

let mk_add_type add_type =
  let add_type_with_jkind
      ?manifest type_ident
      ?(kind=Type_abstract Definition)
      ~jkind
      ?unboxed_jkind
      env =
    let type_uid = Uid.of_predef_id type_ident in
>>>>>>> 5.2.0minus-31
    let type_unboxed_version = match unboxed_jkind with
      | None -> None
      | Some unboxed_jkind ->
        let type_jkind =
          Jkind.of_builtin ~why:(Unboxed_primitive type_ident) unboxed_jkind
        in
        (* All unboxed versions of types explicitly added in the predef are
           abstract, as they are special cased. Other unboxed versions are
           automatically derived. *)
        let type_kind = Type_abstract Definition in
        Some {
          type_params = [];
          type_arity = 0;
          type_kind;
          type_jkind = Jkind.mark_best type_jkind;
          type_loc = Location.none;
          type_private = Asttypes.Public;
          type_manifest = None;
          type_variance = [];
          type_separability = [];
          type_is_newtype = false;
          type_expansion_scope = lowest_level;
          type_attributes = [];
          type_unboxed_default = false;
          type_uid = Uid.unboxed_version type_uid;
          type_unboxed_version = None;
        }
    in
    {type_params = [];
     type_arity = 0;
     type_kind = kind;
     type_jkind = Jkind.mark_best jkind;
     type_loc = Location.none;
     type_private = Asttypes.Public;
     type_manifest = None;
     type_variance = [];
     type_separability = [];
     type_is_newtype = false;
     type_expansion_scope = lowest_level;
     type_attributes = [];
     type_unboxed_default = false;
     type_uid;
     type_unboxed_version;
    }
  in
  let decl1
      ~variance
      ~(param_jkind : jkind_lr)
      ~jkind
      ?(separability = Separability.Ind)
      ?(kind = fun _ -> Type_abstract Definition)
      ()
    =
    let param = newgenvar param_jkind in
    { (decl0 ~jkind:(jkind param) ~kind:(kind param) ()) with
      type_params = [param];
      type_arity = 1;
      type_variance = [variance];
      type_separability = [separability];
    }
  in
  let decl2
      ~variance:(var1, var2)
      ~param_jkinds:(param_jkind1, param_jkind2)
      ~jkind
      ?separability:((sep1, sep2) = (Separability.Ind, Separability.Ind))
      ?(kind = fun _ _ -> Type_abstract Definition)
      ()
    =
    let param1, param2 = newgenvar param_jkind1, newgenvar param_jkind2 in
    { (decl0 ~kind:(kind param1 param2) ~jkind:(jkind param1 param2) ()) with
      type_params = [param1; param2];
      type_arity = 2;
      type_variance = [var1; var2];
      type_separability = [sep1; sep2];
    }
  in
<<<<<<< HEAD
  let variant constrs =
    let mk_elt { cd_args } =
      let sorts = match cd_args with
        | Cstr_tuple args ->
          Misc.Stdlib.Array.of_list_map (fun { ca_sort } -> ca_sort) args
        | Cstr_record lbls ->
          Misc.Stdlib.Array.of_list_map (fun { ld_sort } -> ld_sort) lbls
      in
      Constructor_uniform_value, sorts
||||||| f8c6716f8c
  add_type type_ident decl env

let mk_add_extension add_extension id args =
  List.iter (fun (_, sort) ->
      let raise_error () = Misc.fatal_error
          "sanity check failed: non-value jkind in predef extension \
            constructor; should this have Constructor_mixed shape?" in
      match (sort : Jkind_types.Sort.Const.t) with
      | Base Value -> ()
      | Base (Void | Untagged_immediate | Float32 | Float64 | Word | Bits8 |
             Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512)
      | Univar _ | Product _ -> raise_error ())
    args;
  add_extension id
    { ext_type_path = path_exn;
      ext_type_params = [];
      ext_args =
        Cstr_tuple
          (List.map
            (fun (ca_type, ca_sort) ->
              {
                ca_type;
                ca_sort;
                ca_modalities=Mode.Modality.Const.id;
                ca_loc=Location.none
              })
            args);
      ext_shape = Constructor_uniform_value;
      ext_constant = args = [];
      ext_ret_type = None;
      ext_private = Asttypes.Public;
      ext_loc = Location.none;
      ext_attributes = [Ast_helper.Attr.mk
                          (Location.mknoloc "ocaml.warn_on_literal_pattern")
                          (Parsetree.PStr [])];
      ext_uid = Uid.of_predef_id id;
    }

let variant constrs =
  let mk_elt { cd_args } =
    let sorts = match cd_args with
      | Cstr_tuple args ->
        Misc.Stdlib.Array.of_list_map (fun { ca_sort } -> ca_sort) args
      | Cstr_record lbls ->
        Misc.Stdlib.Array.of_list_map (fun { ld_sort } -> ld_sort) lbls
=======
  add_type type_ident decl env

let mk_add_extension add_extension id args =
  List.iter (fun (_, sort) ->
      let raise_error () = Misc.fatal_error
          "sanity check failed: non-value jkind in predef extension \
            constructor; should this have Constructor_mixed shape?" in
      match (sort : Jkind_types.Sort.Const.t) with
      | Base Value -> ()
      | Base (Void | Untagged_immediate | Float32 | Float64 | Word | Bits8 |
             Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512)
      | Univar _ | Product _ -> raise_error ())
    args;
  add_extension id
    { ext_type_path = path_exn;
      ext_type_params = [];
      ext_args =
        Cstr_tuple
          (List.map
            (fun (ca_type, ca_sort) ->
              {
                ca_type;
                ca_sort;
                ca_modalities=Mode.Modality.Const.id;
                ca_loc=Location.none
              })
            args);
      ext_shape = Constructor_uniform_value;
      ext_constant = args = [];
      ext_ret_type = None;
      ext_private = Asttypes.Public;
      ext_loc = Location.none;
      ext_attributes = [Ast_helper.Attr.mk
                          (Location.mknoloc "ocaml.warn_on_literal_pattern")
                          (Parsetree.PStr [])];
      ext_uid = Uid.of_predef_id id;
    }

let mk_add_jkind add_jkind =
  let add_jkind id jkind env =
    let decl =
      { jkind_manifest = Some jkind;
        jkind_attributes = [];
        jkind_uid = Uid.of_predef_id id;
        jkind_loc = Location.none }
    in
    add_jkind id decl env
  in
  add_jkind

let variant constrs =
  let mk_elt { cd_args } =
    let sorts = match cd_args with
      | Cstr_tuple args ->
        Misc.Stdlib.Array.of_list_map (fun { ca_sort } -> ca_sort) args
      | Cstr_record lbls ->
        Misc.Stdlib.Array.of_list_map (fun { ld_sort } -> ld_sort) lbls
>>>>>>> 5.2.0minus-31
    in
    Type_variant (
      constrs,
      Variant_boxed (Misc.Stdlib.Array.of_list_map mk_elt constrs),
      None)
  in
<<<<<<< HEAD
  let builtin jkind = Jkind.of_builtin ~why:(Primitive type_ident) jkind in
  let builtin1 jkind _param1 = builtin jkind in
  let builtin2 jkind _param1 _param2 = builtin jkind in
  let value_param_jkind =
    Jkind.Builtin.value ~why:(
      Type_argument {
        parent_path = Path.Pident type_ident;
        position = 1;
        arity = 1})
  in
  let value_params_jkind_2 = (
    Jkind.Builtin.value
       ~why:(Type_argument {parent_path = Path.Pident type_ident;
                            position = 1; arity = 2}),
    Jkind.Builtin.value
      ~why:(Type_argument {parent_path = Path.Pident type_ident;
                           position = 2; arity = 2}))
  in
  match tconstr with
  | `Int ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immediate)
       ~unboxed_jkind:Jkind.Const.Builtin.kind_of_untagged_int ()
  | `Char ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immediate)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int8 ()
  | `String ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data) ()
  | `Bytes ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.mutable_data) ()
  | `Float ->
    decl0
      ~jkind:(Jkind.for_float ident_float)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_float
      ()
  | `Floatarray ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.mutable_data) ()
  | `Nativeint ->
    decl0
      ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_nativeint
      ()
  | `Int32 ->
    decl0
      ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int32
      ()
  | `Int64 ->
    decl0
      ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int64
      ()
  | `Extension_constructor ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data) ()
  | `Bool ->
    let kind = variant [cstr ident_false [];
                        cstr ident_true []] in
    decl0 ~kind
      ~jkind:(builtin Jkind.Const.Builtin.immediate)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_bool
      ()
  | `Unit ->
    let kind = variant [cstr ident_void []] in
    decl0 ~kind
      ~jkind:(builtin Jkind.Const.Builtin.immediate)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_unit
      ()
  | `Exn -> decl0 ~kind:Type_open ~jkind:(builtin Jkind.Const.Builtin.exn) ()
  | `Eff ->
    let kind _ = Type_open in
    decl1 ~variance:Variance.full ~kind
      ~jkind:(builtin1 Jkind.Const.Builtin.value)
      ~param_jkind:(Jkind.for_effect_arg ident_eff)
      ()
  | `Continuation ->
    let variance = Variance.(contravariant, covariant) in
    decl2 ~variance ~param_jkinds:value_params_jkind_2
      ~jkind:(builtin2 Jkind.Const.Builtin.value) ()
  | `Array ->
    decl1 ~variance:Variance.full ~param_jkind:Jkind.for_array_argument
||||||| f8c6716f8c
  Type_variant (
    constrs,
    Variant_boxed (Misc.Stdlib.Array.of_list_map mk_elt constrs),
    None)

let unrestricted tvar ca_sort =
  {ca_type=tvar;
   ca_sort;
   ca_modalities=Mode.Modality.Const.id;
   ca_loc=Location.none}

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let build_initial_env add_type add_extension empty_env =
  let add_type_with_jkind, add_type = mk_add_type add_type
  and add_type1 = mk_add_type1 add_type
  and add_type2 = mk_add_type2 add_type
  and add_extension = mk_add_extension add_extension in
  empty_env
  (* Predefined types *)
  |> add_type1 ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
       ~param_jkind:Jkind.for_array_argument
=======
  Type_variant (
    constrs,
    Variant_boxed (Misc.Stdlib.Array.of_list_map mk_elt constrs),
    None)

let unrestricted tvar ca_sort =
  {ca_type=tvar;
   ca_sort;
   ca_modalities=Mode.Modality.Const.id;
   ca_loc=Location.none}

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let build_initial_env add_type add_extension add_jkind empty_env =
  let add_type_with_jkind, add_type = mk_add_type add_type
  and add_type1 = mk_add_type1 add_type
  and add_type2 = mk_add_type2 add_type
  and add_extension = mk_add_extension add_extension
  and add_jkind = mk_add_jkind add_jkind
  in
  empty_env
  (* Predefined types *)
  |> add_type1 ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
       ~param_jkind:Jkind.for_array_argument
>>>>>>> 5.2.0minus-31
       ~jkind:(fun param ->
         Jkind.Builtin.mutable_data ~why:(Primitive ident_array) |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Const.id
           ~type_expr:param) ()
  | `Atomic_loc
    ->
      decl1 ~variance:Variance.full
       ~param_jkind:(
         Jkind.Builtin.value_or_null ~why:(Primitive ident_atomic_loc))
       ~jkind:(fun param ->
         Jkind.Builtin.sync_data ~why:(Primitive ident_atomic_loc) |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Const.id
           ~type_expr:param)
       ()
  | `Iarray ->
      decl1 ~variance:Variance.covariant
       ~param_jkind:Jkind.for_array_argument
       ~jkind:(fun param ->
         Jkind.Builtin.immutable_data ~why:(Primitive ident_iarray) |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Const.id
           ~type_expr:param)
       ()
  | `List ->
      let kind tvar =
        variant [cstr ident_nil [];
                 cstr ident_cons [unrestricted tvar list_argument_sort;
                                  unrestricted (type_list tvar) list_sort]] in
      decl1 ~variance:Variance.covariant ~kind
       ~param_jkind:list_argument_jkind
       ~jkind:list_jkind
        ()
  | `Option ->
      let kind tvar =
        variant [cstr ident_none [];
                 cstr ident_some [unrestricted tvar option_argument_sort]] in
      decl1 ~variance:Variance.covariant ~kind
       ~param_jkind:option_argument_jkind
       ~jkind:(fun param ->
         Jkind.Builtin.immutable_data ~why:Boxed_variant |>
         Jkind.add_with_bounds
           ~modality:Mode.Modality.Const.id
           ~type_expr:param)
        ()
  | `Lazy_t ->
    decl1 ~variance:Variance.covariant
      (* CR layouts v2.8: Can [lazy_t] mode-cross at all? According to Zesen:
         It can at least cross locality, because it's always heap-allocated.
         It might also cross portability, linearity, uniqueness subject to its
         parameter. But I'm also fine not doing that for now (and wait until
         users complains). Internal ticket 5103. *)
      ~param_jkind:value_param_jkind
      ~jkind:(fun _ -> Jkind.for_non_float ~why:(Primitive ident_lazy_t))
      ()
  | `Idx_imm ->
    decl2 ~variance:(Variance.full, Variance.covariant)
       ~param_jkinds:(
         Jkind.Builtin.value ~why:(Type_argument {
           parent_path = Path.Pident ident_idx_imm;
           position = 1;
           arity = 2;
         }),
         Jkind.Builtin.any ~why:(Type_argument {
           parent_path = Path.Pident ident_idx_imm;
           position = 2;
           arity = 2;
         }))
       ~jkind:(builtin2 Jkind.Const.Builtin.kind_of_idx)
       ()
  | `Idx_mut ->
    decl2 ~variance:(Variance.full, Variance.full)
       ~param_jkinds:(
         Jkind.Builtin.value ~why:(Type_argument {
           parent_path = Path.Pident ident_idx_mut;
           position = 1;
           arity = 2;
         }),
         Jkind.Builtin.any ~why:(Type_argument {
           parent_path = Path.Pident ident_idx_mut;
           position = 2;
           arity = 2;
         }))
       ~jkind:(builtin2 Jkind.Const.Builtin.kind_of_idx)
       ()
  | `Lexing_position ->
    decl0
       ~kind:(
         let lbl (field, field_type) =
           let id = Ident.create_predef field in
             {
               ld_id=id;
               ld_mutable=Immutable;
               ld_modalities=Mode.Modality.Const.id;
               ld_type=field_type;
               ld_sort=Jkind_types.Sort.Const.value;
               ld_loc=Location.none;
               ld_attributes=[];
               ld_uid=Uid.of_predef_id id;
             }
         in
         let labels = List.map lbl [
           ("pos_fname", type_string);
           ("pos_lnum", type_int);
           ("pos_bol", type_int);
           ("pos_cnum", type_int) ]
         in
         Type_record (
           labels,
           (Record_boxed (List.map (fun label -> label.ld_sort) labels |> Array.of_list)),
           None
         )
       )
       (* CR layouts v2.8: Possibly remove this -- and simplify [mk_add_type] --
          when we have a better jkind subsumption check. Internal ticket 5104 *)
       ~jkind:Jkind.(
         of_builtin Const.Builtin.immutable_data
           ~why:(Primitive ident_lexing_position) |>
         add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr:type_int |>
         add_with_bounds ~modality:Mode.Modality.Const.id
          ~type_expr:type_string)
       ()
  | `Code ->
    decl1
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~param_jkind:value_param_jkind
       ~jkind:(fun param ->
         Jkind.Builtin.immutable_data ~why:Tquote |>
           Jkind.add_with_bounds
             ~modality:Mode.Modality.Const.id
             ~type_expr:param)
       ()
  | `Int8x16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Int16x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Int32x4 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Int64x2 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Float16x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Float32x4 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Float64x2 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_128bit_vectors ()
  | `Int8x32 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Int16x16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Int32x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Int64x4 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Float16x16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Float32x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Float64x4 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_256bit_vectors ()
  | `Int8x64 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Int16x32 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Int32x16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Int64x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Float16x32 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Float32x16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Float64x8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_512bit_vectors ()
  | `Float32 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immutable_data)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_float32 ()
  | `Int8 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immediate)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int8 ()
  | `Int16 ->
    decl0 ~jkind:(builtin Jkind.Const.Builtin.immediate)
      ~unboxed_jkind:Jkind.Const.Builtin.kind_of_unboxed_int16 ()
  | `Or_null ->
    decl1
      ~variance:Variance.covariant
      (* CR layouts v3: [or_null] is separable only if the argument type
        is non-float. The current separability system can't track that.
        We also want to allow [float or_null] despite it being non-separable.

        For now, we mark the type argument as [Separability.Ind] to permit
        the most argument types, and forbid arrays from accepting [or_null]s.
        In the future, we will track separability in the jkind system. *)
      ~kind:or_null_kind
      ~param_jkind:(Jkind.for_or_null_argument ident_or_null)
      ~jkind:or_null_jkind
      ()

let build_initial_env add_type add_extension empty_env =
  let add_extension id l =
    List.iter (fun (_, sort) ->
        let raise_error () = Misc.fatal_error
            "sanity check failed: non-value jkind in predef extension \
              constructor; should this have Constructor_mixed shape?" in
        match (sort : Jkind_types.Sort.Const.t) with
        | Base Value -> ()
        | Base (Void | Untagged_immediate | Float32 | Float64 | Word | Bits8 |
              Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512)
        | Univar _ | Product _ -> raise_error ())
      l;
    add_extension id
      { ext_type_path = path_exn;
        ext_type_params = [];
        ext_args =
          Cstr_tuple
            (List.map
              (fun (ca_type, ca_sort) ->
                {
                  ca_type;
                  ca_sort;
                  ca_modalities=Mode.Modality.Const.id;
                  ca_loc=Location.none
                })
              l);
        ext_shape = Constructor_uniform_value;
        ext_constant = l = [];
        ext_ret_type = None;
        ext_private = Asttypes.Public;
        ext_loc = Location.none;
        ext_attributes = [Ast_helper.Attr.mk
                            (Location.mknoloc "ocaml.warn_on_literal_pattern")
                            (Parsetree.PStr [])];
        ext_uid = Uid.of_predef_id id;
      }
  in
  List.fold_left (fun env tconstr ->
    add_type (ident_of_type_constr tconstr) (decl_of_type_constr tconstr) env
  ) empty_env all_type_constrs
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
        Jkind_types.Sort.Const.value]
  |> add_extension ident_division_by_zero []
  |> add_extension ident_end_of_file []
  |> add_extension ident_failure [type_string,
       Jkind_types.Sort.Const.value]
  |> add_extension ident_invalid_argument [type_string,
       Jkind_types.Sort.Const.value]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
       Jkind_types.Sort.Const.value]
  |> add_extension ident_not_found []
  |> add_extension ident_out_of_memory []
  |> add_extension ident_out_of_fibers []
  |> add_extension ident_stack_overflow []
  |> add_extension ident_sys_blocked_io []
  |> add_extension ident_sys_error [type_string,
       Jkind_types.Sort.Const.value]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int]),
       Jkind_types.Sort.Const.value]
<<<<<<< HEAD
  |> add_extension ident_continuation_already_taken []

let add_or_null add_type env =
  let tconstr = `Or_null in
  add_type (ident_of_type_constr tconstr) (decl_of_type_constr tconstr) env
||||||| f8c6716f8c
=======
  (* Predefined jkinds *)
  |> add_predef_jkinds add_jkind
>>>>>>> 5.2.0minus-31

let add_simd_stable_extension_types add_type env =
  List.fold_left (fun env tconstr ->
    add_type (ident_of_type_constr tconstr) (decl_of_type_constr tconstr) env
  ) env simd_stable_extension_type_constrs

let add_simd_beta_extension_types _add_type env = env

let add_simd_alpha_extension_types _add_type env = env

let add_small_number_extension_types add_type env =
  List.fold_left (fun env tconstr ->
    add_type (ident_of_type_constr tconstr) (decl_of_type_constr tconstr) env
  ) env small_number_extension_type_constrs

let add_small_number_beta_extension_types _add_type env = env

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
