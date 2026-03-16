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
    cstr_uid: Uid.t;
   }

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

type 'a gen_label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Const.t;(* Modalities on the field *)
    lbl_sort: Jkind_types.Sort.Const.t; (* Sort of the argument *)
    lbl_pos: int;                       (* Position in type *)
    lbl_all: 'a gen_label_description array;   (* All the labels in this type *)
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

let lbl_res_type_path lbl =
  match get_desc lbl.lbl_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
