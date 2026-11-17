(* TEST
 readonly_files = "invalid_variant_.c";
 ocamlopt_opt_exit_status = "2";
 {
   compiler_directory_suffix = ".asan";
   compiler_reference2 =
     "${test_source_directory}/invalid_variant.asan.reference";
   setup-ocamlopt.opt-build-env;
   flags = "-asan -no-frame-pointers -no-stack-checks";
   module = "invalid_variant.ml";
   ocamlopt.opt;
   module = "invalid_variant_.c";
   ocamlopt.opt;
   module = "";
   flags = "-asan -no-frame-pointers -no-stack-checks -a";
   all_modules = "invalid_variant_.c";
   program = "invalid_variant.cmxa";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }{
   arch_amd64;
   compiler_directory_suffix = ".asan-sc";
   compiler_reference2 =
     "${test_source_directory}/invalid_variant.asan-sc.reference";
   setup-ocamlopt.opt-build-env;
   flags = "-asan -frame-pointers -stack-checks";
   module = "invalid_variant.ml";
   ocamlopt.opt;
   module = "invalid_variant_.c";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

(* A selection of axes that no variant has is refused whatever ocamlopt is
   asked to compile for it: a unit, a C file, which would otherwise be
   compiled with the macros and the flags of that selection, and a C file
   on its way into an archive.  The flags give every axis that the message
   depends on, so that the configure default does not matter;
   AddressSanitizer without frame pointers is refused before the platform
   is looked at, and the second selection has frame pointers, which only
   amd64 supports. *)

let answer = 42
