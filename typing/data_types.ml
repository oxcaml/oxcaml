(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Picube, INRIA Paris                          *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: constructor_argument list; (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: tag;                      (* Tag for heap blocks *)
    cstr_repr: variant_representation;  (* Repr of the outer variant *)
    cstr_shape: constructor_representation; (* Repr of the constructor itself *)
    cstr_constant: bool;
    (* True if it's the constructor of a non-[@@unboxed] variant with 0 bits of
       payload. (Or equivalently, if it's represented as either a tagged int or
       the null pointer) *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    (* [Some decl] here iff the cstr has an inline record (which is decl) *)
    cstr_uid: Uid.t;
   }

let equal_tag t1 t2 =
  match (t1, t2) with
  | Ordinary { src_index = i1 }, Ordinary { src_index = i2 } ->
      i2 = i1
  | Extension path1, Extension path2 -> Path.same path1 path2
  | Null, Null -> true
  | (Ordinary _ | Extension _ | Null), _ -> false

let compare_tag t1 t2 =
  match (t1, t2) with
  | Ordinary { src_index = i1 }, Ordinary { src_index = i2 } ->
      Int.compare i1 i2
  | Extension path1, Extension path2 -> Path.compare path1 path2
  | Null, Null -> 0
  | Ordinary _, (Extension _ | Null) -> -1
  | (Extension _ | Null), Ordinary _ -> 1
  | Extension _, Null -> -1
  | Null, Extension _ -> 1

let equal_constr c1 c2 =
  equal_tag c1.cstr_tag c2.cstr_tag

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Extension _, Extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

let cstr_res_type_path cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false

let rec equal_mixed_block_element e1 e2 =
  match e1, e2 with
  | Value, Value | Float64, Float64 | Float32, Float32
  | Float_boxed, Float_boxed | Word, Word
  | Untagged_immediate, Untagged_immediate
  | Bits8, Bits8 | Bits16, Bits16 | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void ->
      true
  | Product es1, Product es2 ->
      Misc.Stdlib.Array.equal equal_mixed_block_element es1 es2
  | (Value | Float64 | Float32 | Float_boxed | Word | Untagged_immediate
    | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512
    | Product _ | Void), _ ->
      false

let rec compare_mixed_block_element e1 e2 =
  match e1, e2 with
  | Value, Value | Float_boxed, Float_boxed
  | Float64, Float64 | Float32, Float32
  | Word, Word | Untagged_immediate, Untagged_immediate
  | Bits8, Bits8 | Bits16, Bits16 | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void ->
      0
  | Product es1, Product es2 ->
      Misc.Stdlib.Array.compare compare_mixed_block_element es1 es2
  | Value, _ -> -1
  | _, Value -> 1
  | Float_boxed, _ -> -1
  | _, Float_boxed -> 1
  | Float64, _ -> -1
  | _, Float64 -> 1
  | Float32, _ -> -1
  | _, Float32 -> 1
  | Word, _ -> -1
  | _, Word -> 1
  | Untagged_immediate, _ -> -1
  | _, Untagged_immediate -> 1
  | Bits8, _ -> -1
  | _, Bits8 -> 1
  | Bits16, _ -> -1
  | _, Bits16 -> 1
  | Bits32, _ -> -1
  | _, Bits32 -> 1
  | Bits64, _ -> -1
  | _, Bits64 -> 1
  | Vec128, _ -> -1
  | _, Vec128 -> 1
  | Vec256, _ -> -1
  | _, Vec256 -> 1
  | Vec512, _ -> -1
  | _, Vec512 -> 1
  | Void, _ -> -1
  | _, Void -> 1

let equal_mixed_product_shape r1 r2 = r1 == r2 ||
  Misc.Stdlib.Array.equal equal_mixed_block_element r1 r2

let equal_constructor_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Constructor_uniform_value, Constructor_uniform_value -> true
  | Constructor_mixed mx1, Constructor_mixed mx2 ->
      equal_mixed_product_shape mx1 mx2
  | (Constructor_mixed _ | Constructor_uniform_value), _ -> false

let equal_variant_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Variant_unboxed, Variant_unboxed ->
      true
  | Variant_boxed cstrs_and_sorts1, Variant_boxed cstrs_and_sorts2 ->
      Misc.Stdlib.Array.equal (fun (cstr1, sorts1) (cstr2, sorts2) ->
          equal_constructor_representation cstr1 cstr2
          && Misc.Stdlib.Array.equal Jkind_types.Sort.Const.equal
               sorts1 sorts2)
        cstrs_and_sorts1
        cstrs_and_sorts2
  | Variant_extensible, Variant_extensible ->
      true
  | Variant_with_null, Variant_with_null -> true
  | (Variant_unboxed | Variant_boxed _ | Variant_extensible | Variant_with_null),
    _ ->
      false

let equal_record_representation r1 r2 = match r1, r2 with
  | Record_unboxed, Record_unboxed ->
      true
  | Record_inlined (tag1, cr1, vr1), Record_inlined (tag2, cr2, vr2) ->
      ignore (cr1 : constructor_representation);
      ignore (cr2 : constructor_representation);
      equal_tag tag1 tag2 && equal_variant_representation vr1 vr2
  | Record_boxed sorts1, Record_boxed sorts2 ->
      Misc.Stdlib.Array.equal Jkind_types.Sort.Const.equal sorts1 sorts2
  | Record_float, Record_float ->
      true
  | Record_ufloat, Record_ufloat ->
      true
  | Record_mixed mx1, Record_mixed mx2 -> equal_mixed_product_shape mx1 mx2
  | (Record_unboxed | Record_inlined _ | Record_boxed _ | Record_float
    | Record_ufloat | Record_mixed _), _ ->
      false

let equal_record_unboxed_product_representation r1 r2 = match r1, r2 with
  | Record_unboxed_product, Record_unboxed_product -> true

type 'a gen_label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Const.t;
    lbl_sort: Jkind_types.Sort.Const.t; (* Sort of the argument *)
    lbl_pos: int;                       (* Position in type *)
    lbl_all: 'a gen_label_description array; (* All the labels in this type *)
    lbl_repres: 'a;                     (* Representation for outer record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

type label_description = record_representation gen_label_description

type unboxed_label_description =
  record_unboxed_product_representation gen_label_description

type _ record_form =
  | Legacy : record_representation record_form
  | Unboxed_product : record_unboxed_product_representation record_form

type record_form_packed =
  | P : _ record_form -> record_form_packed

let record_form_to_string (type rep) (record_form : rep record_form) =
  match record_form with
  | Legacy -> "record"
  | Unboxed_product -> "unboxed record"

let rec mixed_block_element_of_const_sort (sort : Jkind_types.Sort.Const.t) =
  match sort with
  | Base Value -> Value
  | Base Bits8 -> Bits8
  | Base Bits16 -> Bits16
  | Base Bits32 -> Bits32
  | Base Bits64 -> Bits64
  | Base Float32 -> Float32
  | Base Float64 -> Float64
  | Base Untagged_immediate -> Untagged_immediate
  | Base Vec128 -> Vec128
  | Base Vec256 -> Vec256
  | Base Vec512 -> Vec512
  | Base Word -> Word
  | Product sorts ->
      Product
        (Array.map mixed_block_element_of_const_sort (Array.of_list sorts))
  | Base Void -> Void
  | Univar _ -> Misc.fatal_error "mixed_block_element_of_const_sort: Univar"

let lbl_res_type_path lbl =
  match get_desc lbl.lbl_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
