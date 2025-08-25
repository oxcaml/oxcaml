(* TEST
 readonly_files = "\
   bits_u_monoid.mli \
   bits_u_monoid_utils.ml bits_u_monoid_utils.mli \
   float_u_monoid.ml float_u_monoid.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "bits_u_monoid.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Bits_u_monoid";
 module = "float_u_monoid.mli float_u_monoid.ml";
 ocamlopt.byte;

 flags = "-drawflambda -parameter Bits_u_monoid";
 module = "bits_u_monoid_utils.mli bits_u_monoid_utils.ml";
 ocamlopt.byte;

 flags = "-drawflambda -instantiate";
 module = "";
 program = "bits_u_monoid_utils-Float_u_monoid.cmx";
 all_modules = "bits_u_monoid_utils.cmx float_u_monoid.cmx";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute";
 module = "test_float_u_monoid.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "test_float_u_monoid.exe";
 all_modules = "\
   float_u_monoid.cmx \
   bits_u_monoid_utils.cmx \
   bits_u_monoid_utils-Float_u_monoid.cmx \
   test_float_u_monoid.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/test_float_u_monoid.exe";
 run;

 check-program-output;
*)

external to_float : float# -> float = "%float_of_float#"

module M =
  Bits_u_monoid_utils(Bits_u_monoid)(Float_u_monoid) [@jane.non_erasable.instances]

let _ = print_float (to_float (M.pow #2.0 10))
let _ = print_endline ""
