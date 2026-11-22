(* TEST
 readonly_files = "commandline_variant_unit.ml";
 codegen_variant = "+sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 commandline = "-c -pp '${cc} -E -P -x c ${cflags} -I${runtime}' commandline_variant_unit.ml";
 ocamlopt.byte;
 commandline = "";
 binary_modules = "commandline_variant_unit";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 run;
*)

(* A native compiler that a test gives its whole command line compiles for
   the variant of the block, as it does in a script: the command line
   above selects no variant and names no library tree.  The link that
   follows is the assertion: it is for the variant of the block and would
   refuse a unit compiled for another one.

   The command line is also read as a script is: cflags ends with the C
   flags of the variant.  The unit goes through the C preprocessor with
   them, and records whether it saw the macro of the variant's axis. *)

let () = assert Commandline_variant_unit.stack_checks_in_c
