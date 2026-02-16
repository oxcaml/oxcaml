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
    cstr_constant: bool;                (* True if all args are void *)
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

(* Constructors are the same *)
val equal_tag :  tag -> tag -> bool

(* Comparison of tags to store them in sets. *)
val compare_tag :  tag -> tag -> int

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
    constructor_description ->  constructor_description -> bool

(* Type constructor of the constructor's result type. *)
val cstr_res_type_path : constructor_description -> Path.t

(* Equality *)

val equal_record_representation :
  record_representation -> record_representation -> bool

val equal_record_unboxed_product_representation :
  record_unboxed_product_representation -> record_unboxed_product_representation -> bool

val equal_variant_representation :
  variant_representation -> variant_representation -> bool

type 'a gen_label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Const.t;
                                        (* Modalities on the field *)
    lbl_sort: Jkind_types.Sort.Const.t; (* Sort of the argument *)
    lbl_pos: int;                       (* Position in type *)
    lbl_all: 'a gen_label_description array;   (* All the labels in this type *)
    lbl_repres: 'a;  (* Representation for outer record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

type label_description = record_representation gen_label_description

type unboxed_label_description = record_unboxed_product_representation gen_label_description

(** This type tracks the distinction between legacy records ([{ field }]) and unboxed
    records ([#{ field }]). Note that [Legacy] includes normal boxed records, as well as
    inlined and [[@@unboxed]] records.

    As a GADT, it also lets us avoid duplicating functions that handle both record forms,
    such as [Env.find_label_by_name], which has type
    ['rep record_form -> Longident.t -> Env.t -> 'rep gen_label_description].
*)
type _ record_form =
  | Legacy : record_representation record_form
  | Unboxed_product : record_unboxed_product_representation record_form

type record_form_packed =
  | P : _ record_form -> record_form_packed

val record_form_to_string : _ record_form -> string

(* Type constructor of the label record type. *)
val lbl_res_type_path : label_description -> Path.t

val mixed_block_element_of_const_sort :
  Jkind_types.Sort.Const.t -> mixed_block_element
