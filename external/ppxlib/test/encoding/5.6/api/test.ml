open Ppxlib_ast

module To_ocaml = Convert (Js) (Compiler_version)
module From_ocaml = Convert (Compiler_version) (Js)

open Ppxlib

#install_printer Pprintast.core_type;;
#install_printer Pprintast.expression;;
#install_printer Pprintast.pattern;;

module Builder = Ast_builder.Make (struct
  let loc = Location.none
end)

(* Generate an encoded expression hole: [f _] *)
let encoded_pexp_hole =
  let f = { txt = Lident "f"; loc = Location.none } in
  Builder.pexp_apply (Builder.pexp_ident f) [ (Nolabel, Builder.pexp_hole ()) ];;

(* Migrate it to the current compiler (>= 5.6, as per dune rules) *)
let expr_hole = To_ocaml.copy_expression encoded_pexp_hole;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.expression expr_hole;;
[%%expect{|
val as_source : string = "f _"
|}]

(* Migrate back to ppxlib's AST *)
let encoded_by_migration = From_ocaml.copy_expression expr_hole

let pattern =
  let arg_hole = Ast_pattern.(pair nolabel pexp_hole) in
  Ast_pattern.(pexp_apply (pexp_ident __) (arg_hole ^:: nil));;
[%%ignore]

(* Destruct both the migration and Ast_builder generated encodings with
   the Ast_pattern function. *)
let destruct_from_migration =
  Ast_pattern.parse pattern Location.none encoded_by_migration
    (fun l _ () -> l)
    () ();;
[%%expect{|
val destruct_from_migration : longident = Ppxlib__.Import.Lident "f"
|}]

let destruct =
  Ast_pattern.parse pattern Location.none encoded_pexp_hole
    (fun x _ () -> x)
    () ();;
[%%expect{|
val destruct : longident = Ppxlib__.Import.Lident "f"
|}]

(* -------- Tests for module expression holes -------- *)

(* Generate an encoded expression hole: [f _] *)
let encoded_pmod_hole =
  let f = { txt = Lident "F"; loc = Location.none } in
  Builder.pmod_apply (Builder.pmod_ident f) (Builder.pmod_hole ());;

(* Migrate it to the current compiler (>= 5.6, as per dune rules) *)
let mod_hole = To_ocaml.copy_module_expr encoded_pmod_hole;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.module_expr mod_hole;;
[%%expect{|
val as_source : string = "(F)(_)"
|}]

(* Migrate back to ppxlib's AST *)
let encoded_by_migration_hole = From_ocaml.copy_module_expr mod_hole;;

let pattern =
  Ast_pattern.(pmod_apply (pmod_ident __) pmod_hole);;
[%%ignore]

(* Destruct both the migration and Ast_builder generated encodings with
   the Ast_pattern function. *)
let destruct_from_migration =
  Ast_pattern.parse pattern Location.none encoded_by_migration_hole
    (fun l _ () -> l)
    () ();;
[%%expect{|
val destruct_from_migration : longident = Ppxlib__.Import.Lident "F"
|}]

let destruct =
  Ast_pattern.parse pattern Location.none encoded_pmod_hole
    (fun x _ () -> x)
    () ();;
[%%expect{|
val destruct : longident = Ppxlib__.Import.Lident "F"
|}]
