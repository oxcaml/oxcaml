(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                       Guillaume Bury, OCamlPro SAS                     *)
(*                                                                        *)
(*   Copyright 2017--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Values_or_immediates_or_naked_floats
  | Unboxed_products
  | Naked_float32s
  | Naked_ints
  | Naked_int8s
  | Naked_int16s
  | Naked_int32s
  | Naked_int64s
  | Naked_nativeints
  | Naked_vec128s
  | Naked_vec256s
  | Naked_vec512s

let print ppf t =
  match t with
  | Values_or_immediates_or_naked_floats -> Format.pp_print_string ppf "regular"
  | Unboxed_products -> Format.pp_print_string ppf "Unboxed_products"
  | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
  | Naked_ints -> Format.pp_print_string ppf "Naked_ints"
  | Naked_int8s -> Format.pp_print_string ppf "Naked_int8s"
  | Naked_int16s -> Format.pp_print_string ppf "Naked_int16s"
  | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
  | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
  | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"
  | Naked_vec128s -> Format.pp_print_string ppf "Naked_vec128s"
  | Naked_vec256s -> Format.pp_print_string ppf "Naked_vec256s"
  | Naked_vec512s -> Format.pp_print_string ppf "Naked_vec512s"

let compare = Stdlib.compare

let of_element_kind t =
  (* This is used for reification, and we don't yet handle unboxed product
     arrays there. *)
  match (t : Flambda_kind.t) with
  | Value | Naked_number Naked_float -> Values_or_immediates_or_naked_floats
  | Naked_number Naked_float32 -> Naked_float32s
  | Naked_number Naked_immediate -> Naked_ints
  | Naked_number Naked_int8 -> Naked_int8s
  | Naked_number Naked_int16 -> Naked_int16s
  | Naked_number Naked_int32 -> Naked_int32s
  | Naked_number Naked_int64 -> Naked_int64s
  | Naked_number Naked_nativeint -> Naked_nativeints
  | Naked_number Naked_vec128 -> Naked_vec128s
  | Naked_number Naked_vec256 -> Naked_vec256s
  | Naked_number Naked_vec512 -> Naked_vec512s
  | Region | Rec_info ->
    Misc.fatal_errorf
      "Arrays cannot contain elements of kind region or rec_info"

let of_lambda array_kind =
  match (array_kind : Lambda.array_kind) with
  | Pgenarray | Paddrarray | Pgcignorableaddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray Unboxed_float64 ->
    Values_or_immediates_or_naked_floats
  | Punboxedfloatarray Unboxed_float32 -> Naked_float32s
  | Punboxedoruntaggedintarray Untagged_int -> Naked_ints
  | Punboxedoruntaggedintarray Untagged_int8 -> Naked_int8s
  | Punboxedoruntaggedintarray Untagged_int16 -> Naked_int16s
  | Punboxedoruntaggedintarray Unboxed_int32 -> Naked_int32s
  | Punboxedoruntaggedintarray Unboxed_int64 -> Naked_int64s
  | Punboxedoruntaggedintarray Unboxed_nativeint -> Naked_nativeints
  | Punboxedvectorarray Unboxed_vec128 -> Naked_vec128s
  | Punboxedvectorarray Unboxed_vec256 -> Naked_vec256s
  | Punboxedvectorarray Unboxed_vec512 -> Naked_vec512s
  | Pgcscannableproductarray _ | Pgcignorableproductarray _ -> Unboxed_products
