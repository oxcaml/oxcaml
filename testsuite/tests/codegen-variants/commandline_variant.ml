(* TEST
 readonly_files = "sc_unit.ml";
 codegen_variant = "sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 commandline = "-c sc_unit.ml";
 ocamlopt.byte;
 commandline = "";
 binary_modules = "sc_unit";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A native compiler that a test gives its whole command line compiles for
   the variant of the block, as it does in a script: the command line
   above selects no variant and names no library tree.  The link that
   follows is the assertion: it is for the variant of the block and would
   refuse a unit compiled for another one. *)

let () = assert (Sc_unit.f () = 42)
