(* TEST
 include ocamlcommon;
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   native;
 } {
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   native;
 } {
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 } {
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   native;
 } {
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 } {
   codegen_variant = "fp-poll";
   variant_available;
   compiler_directory_suffix = ".fp-poll";
   native;
 } {
   codegen_variant = "poll-sc";
   variant_available;
   compiler_directory_suffix = ".poll-sc";
   native;
 } {
   codegen_variant = "fp-poll-sc";
   variant_available;
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 } {
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   native;
 } {
   codegen_variant = "asan-fp-poll";
   variant_available;
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

(* [include ocamlcommon] names directories of the default codegen variant.
   For a block that selects a variant, ocamltest resolves them in that
   variant's library tree, whose compiler-libs directory has the variant's
   own build of the compiler's libraries: a program that uses them can be
   built for every variant.  With the default variant's build, the
   compilation or the link would be refused. *)

let () =
  let expression = Parse.expression (Lexing.from_string "f x + 1") in
  Format.printf "%a@." Pprintast.expression expression
