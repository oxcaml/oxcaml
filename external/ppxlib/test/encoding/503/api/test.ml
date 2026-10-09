open Ppxlib_ast

module To_ocaml = Convert (Js) (Compiler_version)
module From_ocaml = Convert (Compiler_version) (Js)

open Ppxlib

#install_printer Pprintast.pattern;;

module Builder = Ast_builder.Make(struct let loc = Location.none end)

let effect_construct =
  Builder.(ppat_construct
             (Located.mk (Longident.parse "Xchg"))
             (Some (ppat_var (Located.mk "n"))))

(* Generate an encoded effect pattern *)
let encoded_effect_pattern =
  Builder.ppat_effect
    effect_construct
    Builder.(ppat_var (Located.mk "k"))

(* Migrate it to the current compiler (>= 5.3, as per dune rules) *)
let effect_pattern = To_ocaml.copy_pattern encoded_effect_pattern
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.pattern effect_pattern;;
[%%expect{|
val as_source : string = "effect Xchg n,  k"
|}]

(* Migrate back to ppxlib's AST *)
let encoded_by_migration = From_ocaml.copy_pattern effect_pattern

let pattern = Ast_pattern.(ppat_effect __ __)
[%%ignore]

let destruct_from_migration =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration
    (fun effect_ k -> (effect_, k))
[%%expect{|
val destruct_from_migration :
  (pattern * pattern, Location.Error.t Stdppx.NonEmptyList.t) result =
  Ok (Xchg n, k)
|}]

let destruct =
  Ast_pattern.parse_res pattern Location.none encoded_effect_pattern
    (fun effect_ k -> (effect_, k))
[%%expect{|
val destruct :
  (pattern * pattern, Location.Error.t Stdppx.NonEmptyList.t) result =
  Ok (Xchg n, k)
|}]
