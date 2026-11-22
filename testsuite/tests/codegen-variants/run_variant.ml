(* TEST
 readonly_files = "sc_unit.ml";
 codegen_variant = "sc";
 variant_available;
 setup-ocamlopt.byte-build-env;
 program = "${ocamlrun}";
 arguments = "${ocamlopt_byte} -c sc_unit.ml";
 run;
 program = "${test_build_directory}/run_variant.exe";
 arguments = "";
 binary_modules = "sc_unit";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* A native compiler that a test starts with the run action compiles for
   the variant of the block, as it does in a script: its arguments select
   no variant and name no library tree.  The link that follows is the
   assertion: it is for the variant of the block and would refuse a unit
   compiled for another one. *)

let () = assert (Sc_unit.f () = 42)
