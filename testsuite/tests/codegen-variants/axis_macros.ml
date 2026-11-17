(* TEST
 modules = "axis_macros_.c";
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

(* A C stub compiled by ocamlopt sees exactly the axis macros of the codegen
   variant being compiled, whatever the configure default is: each block
   compiles the stub for one variant and prints the macros it found
   defined. *)

external axis_macros : unit -> string = "axis_macros"

let () = Printf.printf "defined:%s\n" (axis_macros ())
