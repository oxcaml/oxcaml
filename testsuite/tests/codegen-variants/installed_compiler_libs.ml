(* TEST
 flags = "-I +compiler-libs";
 libraries = "ocamlcommon";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   native;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   native;
 }{
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 }{
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   native;
 }{
   codegen_variant = "fp-poll";
   variant_available;
   compiler_directory_suffix = ".fp-poll";
   native;
 }{
   codegen_variant = "poll-sc";
   variant_available;
   compiler_directory_suffix = ".poll-sc";
   native;
 }{
   codegen_variant = "fp-poll-sc";
   variant_available;
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 }{
   codegen_variant = "asan-fp-poll";
   variant_available;
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

(* A program that links the compiler's own libraries can be built for every
   codegen variant of one installation: the library tree of each variant
   has its own build of them (the linker would refuse the base
   variant's). *)

let () =
  let structure = Parse.implementation (Lexing.from_string "let x = 1 + 2") in
  Printf.printf "parsed %d structure item\n" (List.length structure)
