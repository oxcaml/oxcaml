(* Conv_error: Module for Handling Errors during Automated S-expression
   Conversions *)

open StdLabels
open Printf
open Sexp_conv

exception Of_sexp_error = Of_sexp_error

let error ~loc ~sexp msg = of_sexp_error (sprintf "%s_of_sexp: %s" loc msg) sexp
let simple_error msg loc sexp = error ~loc ~sexp msg

(* Errors concerning tuples *)

let tuple_of_size_n_expected loc n sexp =
  error ~loc ~sexp (sprintf "tuple of size %d expected" n)
;;

let tuple_pair_expected loc name sexp =
  let msg = sprintf "%s_of_sexp: expected a pair beginning with label %s" loc name in
  of_sexp_error msg sexp
;;

let tuple_incorrect_label loc name pos sexp =
  let msg =
    sprintf "%s_of_sexp: incorrect label for element %s at position %i" loc name pos
  in
  of_sexp_error msg sexp
;;

(* Errors concerning sum types *)

let stag_no_args = simple_error "this constructor does not take arguments"

let stag_incorrect_n_args loc tag sexp =
  error ~loc ~sexp (sprintf "sum tag %S has incorrect number of arguments" tag)
;;

let stag_takes_args = simple_error "this constructor requires arguments"
let nested_list_invalid_sum = simple_error "expected a variant type, saw a nested list"
let empty_list_invalid_sum = simple_error "expected a variant type, saw an empty list"
let unexpected_stag loc _ sexp = simple_error "unexpected variant constructor" loc sexp

(* Errors concerning records *)

let record_sexp_bool_with_payload =
  simple_error "record conversion: a [sexp.bool] field was given a payload."
;;

let record_only_pairs_expected =
  simple_error
    "record conversion: only pairs expected, their first element must be an atom"
;;

let record_invalid_fields ~what ~loc fld_names sexp =
  let fld_names_str = String.concat fld_names ~sep:" " in
  error ~loc ~sexp (sprintf "%s: %s" what fld_names_str)
;;

let record_duplicate_fields loc fld_names sexp =
  record_invalid_fields ~what:"duplicate fields" ~loc fld_names sexp
;;

let record_extra_fields loc fld_names sexp =
  record_invalid_fields ~what:"extra fields" ~loc fld_names sexp
;;

let rec record_get_undefined_loop fields = function
  | [] -> String.concat (List.rev fields) ~sep:" "
  | (true, field) :: rest -> record_get_undefined_loop (field :: fields) rest
  | _ :: rest -> record_get_undefined_loop fields rest
;;

let record_undefined_elements loc sexp lst =
  let undefined = record_get_undefined_loop [] lst in
  let msg = sprintf "the following record elements were undefined: %s" undefined in
  error ~loc ~sexp msg
;;

let record_list_instead_atom = simple_error "list instead of atom for record expected"

let record_poly_field_value =
  simple_error "cannot convert values of types resulting from polymorphic record fields"
;;

(* Errors concerning polymorphic variants *)

exception No_variant_match

let no_variant_match () = raise No_variant_match
let no_matching_variant_found = simple_error "no matching variant found"
let ptag_no_args = simple_error "polymorphic variant does not take arguments"

let ptag_incorrect_n_args loc cnstr sexp =
  error
    ~loc
    ~sexp
    (sprintf "polymorphic variant tag %S has incorrect number of arguments" cnstr)
;;

let ptag_takes_args = simple_error "polymorphic variant tag takes an argument"

let nested_list_invalid_poly_var =
  simple_error "a nested list is an invalid polymorphic variant"
;;

let empty_list_invalid_poly_var =
  simple_error "the empty list is an invalid polymorphic variant"
;;

let empty_type = simple_error "trying to convert an empty type"
