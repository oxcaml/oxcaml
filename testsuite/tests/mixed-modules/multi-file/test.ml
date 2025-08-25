(* TEST
 readonly_files = "\
   numbers.ml \
   ring.ml ring.mli \
   ring_utils.ml ring_utils.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "";
 module = "ring.mli ring.ml"; (* mixed toplevel module *)
 ocamlopt.byte;

 flags = "";
 module = "numbers.ml";
  (* mixed toplevel module that depends on another mixed toplevel module*)
 all_modules = "ring.cmx";
 ocamlopt.byte;

 flags = "";
 module = "ring_utils.mli ring_utils.ml";
  (* value-only toplevel module that depends on a mixed toplevel module*)
 all_modules = "ring.cmx";
 ocamlopt.byte;

 flags = "";
 module = "test.ml";
 all_modules = "ring.cmx ring_utils.cmx numbers.cmx";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "test.exe";
 all_modules = "ring.cmx numbers.cmx ring_utils.cmx test.cmx";
 ocamlopt.byte;

 program = "${test_build_directory}/test.exe";
 run;

 check-program-output;
*)

let _ = print_int (Ring.to_int (Ring_utils.pow Numbers.five 4))
let _ = print_endline ""
let _ = print_int (Ring.to_int (Ring_utils.pow Numbers.two 10))
let _ = print_endline ""
