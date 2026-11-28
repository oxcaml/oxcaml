(* TEST
 modules = "header_path_.c";
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 flags = "-I ${ocamlsrcdir}/stdlib/caml \
   -ccopt -UCAML_CODEGEN_VARIANT_EXPLICIT -ccopt -UWITH_FRAME_POINTERS \
   -ccopt -USTACK_CHECKS_ENABLED -ccopt -UWITH_ADDRESS_SANITIZER";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   reference = "${test_source_directory}/axis_macros.base.reference";
   native;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   reference = "${test_source_directory}/axis_macros.fp.reference";
   native;
 }{
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   reference = "${test_source_directory}/axis_macros.sc.reference";
   native;
 }{
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   reference = "${test_source_directory}/axis_macros.base.reference";
   native;
 }{
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   reference = "${test_source_directory}/axis_macros.fp-sc.reference";
   native;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   reference = "${test_source_directory}/axis_macros.asan-fp.reference";
   native;
 }
*)

(* A directory of C headers named by a path inside the library root
   (here the root OCAMLLIB names) is looked up in the tree of the variant
   being compiled, like any other -I directory.  The stub includes m.h
   through that directory only, and the flags take back the axis macros
   that ocamlopt defines for every .c file it compiles, so that what the
   stub sees is what the header it reached defines: each variant tree's m.h
   describes that variant. *)

external header_macros : unit -> string = "header_macros"

let () = Printf.printf "defined:%s\n" (header_macros ())
