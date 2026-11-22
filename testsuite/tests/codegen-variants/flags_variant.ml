(* TEST
 readonly_files = "commandline_variant_unit.ml";
 codegen_variant = "+sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 preprocessor = "-pp '${cc} -E -P -x c ${cflags} -I${runtime}'";
 {
   module = "commandline_variant_unit.ml";
   flags = "${preprocessor}";
   ocamlopt.byte;
   module = "";
   flags = "";
   binary_modules = "commandline_variant_unit";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
 }{
   module = "commandline_variant_unit.ml";
   ocamlopt_flags = "${preprocessor}";
   ocamlopt.byte;
   module = "";
   ocamlopt_flags = "";
   binary_modules = "commandline_variant_unit";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
 }{
   all_modules = "commandline_variant_unit.ml";
   compile_only = "true";
   last_flags = "${preprocessor}";
   ocamlopt.byte;
   all_modules = "flags_variant.ml";
   compile_only = "false";
   last_flags = "";
   binary_modules = "commandline_variant_unit";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
 }
*)

(* The flags that a test gives a native compiler are read as a command
   line that it writes itself is (see commandline_variant.ml): cflags ends
   with the C flags of the variant of the block.  The compiler states the
   variant for the C files that it compiles, and for nothing else that
   the flags do with cflags: here a unit goes through the C preprocessor
   with them, and records whether it saw the macro of the variant's axis.
   Each block gives the preprocessor in another of the three variables. *)

let () = assert Commandline_variant_unit.stack_checks_in_c
