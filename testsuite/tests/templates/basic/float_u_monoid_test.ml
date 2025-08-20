(* TEST
 readonly_files = "\
   bits_u_monoid.mli \
   bits_u_monoid_utils.ml bits_u_monoid_utils.mli \
   float_u_monoid.ml float_u_monoid.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter -I +stdlib_upstream_compatible";
 module = "bits_u_monoid.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Bits_u_monoid -I +stdlib_upstream_compatible";
 module = "float_u_monoid.mli float_u_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Bits_u_monoid -I +stdlib_upstream_compatible";
 module = "bits_u_monoid_utils.mli bits_u_monoid_utils.ml";
 ocamlopt.byte;

 flags = "-dlambda -I +stdlib_upstream_compatible -instantiate";
 module = "";
 program = "bits_u_monoid_utils-Float_u_monoid.cmx";
 all_modules = "bits_u_monoid_utils.cmx float_u_monoid.cmx";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute -I +stdlib_upstream_compatible";
 module = "float_u_monoid_test.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "-I +stdlib_upstream_compatible stdlib_upstream_compatible.cmxa";
 module = "";
 program = "float_u_monoid_test.exe";
 all_modules = "\
   float_u_monoid.cmx \
   bits_u_monoid_utils.cmx \
   bits_u_monoid_utils-Float_u_monoid.cmx \
   float_u_monoid_test.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/float_u_monoid_test.exe";
 run;

 check-program-output;
*)

module M =
  Bits_u_monoid_utils(Bits_u_monoid)(Float_u_monoid) [@jane.non_erasable.instances]

let _ = print_float (Stdlib_upstream_compatible.Float_u.to_float (M.pow #2.0 10))
let _ = print_endline ""
