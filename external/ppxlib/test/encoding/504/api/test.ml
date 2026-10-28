open Ppxlib_ast

module To_ocaml = Convert (Js) (Compiler_version)
module From_ocaml = Convert (Compiler_version) (Js)

open Ppxlib

#install_printer Pprintast.core_type;;
#install_printer Pprintast.expression;;
#install_printer Pprintast.pattern;;

module Builder = Ast_builder.Make(struct let loc = Location.none end)

let ptyp_int = Builder.(ptyp_constr (Located.mk (Longident.parse "int")) [])
let ptyp_string =
  Builder.(ptyp_constr (Located.mk (Longident.parse "string")) []);;
[%%ignore]

(* Generate an encoded labeled tuple type *)
let encoded_labeled_tuple_type =
  Builder.ptyp_labeled_tuple
    [ Some "a", ptyp_int
    ; Some "b", ptyp_int
    ; None, ptyp_string
    ]

(* Migrate it to the current compiler (>= 5.4, as per dune rules) *)
let labeled_tuple_type = To_ocaml.copy_core_type encoded_labeled_tuple_type;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.core_type labeled_tuple_type;;
[%%expect{|
val as_source : string = "(a:int * b:int * string)"
|}]

(* Migrate back to ppxlib's AST *)
let encoded_by_migration = From_ocaml.copy_core_type labeled_tuple_type

let pattern = Ast_pattern.(ptyp_labeled_tuple __);;
[%%ignore]

(* Destruct both the migration and Ast_builder generated encodings with
   the Ast_pattern function. *)
let destruct_from_migration =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration (fun x -> x);;
[%%expect{|
val destruct_from_migration :
  ((string option * core_type) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", int); (Some "b", int); (None, string)]
|}]

let destruct =
  Ast_pattern.parse_res pattern Location.none
    encoded_labeled_tuple_type (fun x -> x);;
[%%expect{|
val destruct :
  ((string option * core_type) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", int); (Some "b", int); (None, string)]
|}]

(* -------- Same tests with labeled tuples expressions ---------- *)

let encoded_labeled_tuple_expr =
  Builder.pexp_labeled_tuple
    [ Some "a", Builder.eint 0
    ; Some "b", Builder.eint 1
    ; None, Builder.estring "abc"
    ]

let labeled_tuple_expr = To_ocaml.copy_expression encoded_labeled_tuple_expr;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.expression
    labeled_tuple_expr;;
[%%expect{|
val as_source : string = "(~a:0, ~b:1, \"abc\")"
|}]

let encoded_by_migration = From_ocaml.copy_expression labeled_tuple_expr

let pattern = Ast_pattern.(pexp_labeled_tuple __);;
[%%ignore]

let destruct_from_migration =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration
    (fun x -> x);;
[%%expect{|
val destruct_from_migration :
  ((string option * expression) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", 0); (Some "b", 1); (None, "abc")]
|}]

let destruct =
  Ast_pattern.parse_res pattern Location.none encoded_labeled_tuple_expr
    (fun x -> x);;
[%%expect{|
val destruct :
  ((string option * expression) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", 0); (Some "b", 1); (None, "abc")]
|}]

(* -------- Same tests with labeled tuples patterns ---------- *)

let encoded_labeled_tuple_pat =
  Builder.ppat_labeled_tuple
    [ Some "a", Builder.(ppat_var (Located.mk "a"))
    ; Some "b", Builder.ppat_any
    ; None, Builder.(ppat_var (Located.mk "c"))
    ]
    Open

let labeled_tuple_pat = To_ocaml.copy_pattern encoded_labeled_tuple_pat;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.pattern labeled_tuple_pat;;
[%%expect{|
val as_source : string = "(~a, ~b:_, c, ..)"
|}]

let encoded_by_migration = From_ocaml.copy_pattern labeled_tuple_pat

let pattern = Ast_pattern.(ppat_labeled_tuple __);;
[%%ignore]

let destruct_from_migration =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration
    (fun x -> x);;
[%%expect{|
val destruct_from_migration :
  ((string option * pattern) list * closed_flag,
   Location.Error.t Stdppx.NonEmptyList.t)
  result =
  Ok ([(Some "a", a); (Some "b", _); (None, c)], Ppxlib__.Import.Open)
|}]

let destruct =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration
    (fun x -> x);;
[%%expect{|
val destruct :
  ((string option * pattern) list * closed_flag,
   Location.Error.t Stdppx.NonEmptyList.t)
  result =
  Ok ([(Some "a", a); (Some "b", _); (None, c)], Ppxlib__.Import.Open)
|}]
