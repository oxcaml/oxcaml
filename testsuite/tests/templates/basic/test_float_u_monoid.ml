(* TEST
 readonly_files = "\
   bits_u_monoid.mli \
   monoid.mli \
   bits_u_monoid_utils.ml bits_u_monoid_utils.mli \
   bits_u_monoid_utils_2.ml bits_u_monoid_utils_2.mli \
   float_u_monoid.ml float_u_monoid.mli \
   string_monoid.ml string_monoid.mli \
   float_u_with_monoid.ml float_u_with_monoid.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "bits_u_monoid.mli";
 ocamlopt.byte;

 flags = "-as-parameter";
 module = "monoid.mli";
 ocamlc.byte;

 flags = "-as-argument-for Bits_u_monoid";
 module = "float_u_monoid.mli float_u_monoid.ml";
 ocamlopt.byte;

 flags = "-as-argument-for Monoid";
 module = "string_monoid.mli string_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Bits_u_monoid";
 module = "bits_u_monoid_utils.mli bits_u_monoid_utils.ml";
 ocamlopt.byte;

 flags = "-parameter Bits_u_monoid";
 module = "bits_u_monoid_utils_2.mli bits_u_monoid_utils_2.ml";
 ocamlopt.byte;

 flags = "-parameter Monoid";
 module = "float_u_with_monoid.mli float_u_with_monoid.ml";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "bits_u_monoid_utils-Float_u_monoid.cmx";
  (* Value-only module that takes a mixed module as a parameter. *)
 all_modules = "bits_u_monoid_utils.cmx float_u_monoid.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "bits_u_monoid_utils_2-Float_u_monoid.cmx";
  (* Mixed module that takes a mixed module as a parameter. *)
 all_modules = "bits_u_monoid_utils_2.cmx float_u_monoid.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "float_u_with_monoid-String_monoid.cmx";
  (* Mixed module that takes a value-only module as a parameter. *)
 all_modules = "float_u_with_monoid.cmx string_monoid.cmx";
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
   string_monoid.cmx \
   bits_u_monoid_utils.cmx \
   bits_u_monoid_utils_2.cmx \
   float_u_with_monoid.cmx \
   bits_u_monoid_utils-Float_u_monoid.cmx \
   bits_u_monoid_utils_2-Float_u_monoid.cmx \
   float_u_with_monoid-String_monoid.cmx \
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

module M_2 =
  Bits_u_monoid_utils_2(Bits_u_monoid)(Float_u_monoid) [@jane.non_erasable.instances]

module M_3 =
  Float_u_with_monoid(Monoid)(String_monoid) [@jane.non_erasable.instances]

let _ = print_float (to_float (M.pow #5.0 4))
let _ = print_endline ""
let _ = print_float (to_float (M_2.pow #2.0 10))
let _ = print_endline ""
let _ = print_float (to_float M_2.one)
let _ = print_endline ""
let _ =
  let #(a, b) = M_3.append #(#6.0, "hello ") #(#7.0, "world") in
  print_float (to_float (a));
  print_endline "";
  print_endline b
