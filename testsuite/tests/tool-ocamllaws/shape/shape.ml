(* TEST
 flags = "-extension laws";
 {
   setup-ocamlopt.opt-build-env;
   module = "shape.ml";
   ocamlopt.opt;
   script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
     -format laws-file -o shape_laws.ml shape.cmx";
   script;
   output = "${test_build_directory}/shape_laws.ml";
   reference = "${test_source_directory}/shape_laws.ml.reference";
   check-program-output;
   module = "shape_laws.ml";
   ocamlopt.opt;
 }{
   setup-ocamlc.byte-build-env;
   module = "shape.ml";
   ocamlc.byte;
   script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
     -format laws-file -o shape_laws.ml shape.cmo";
   script;
   output = "${test_build_directory}/shape_laws.ml";
   reference = "${test_source_directory}/shape_laws.ml.reference";
   check-program-output;
 }
*)

(* This test fixes the shape of the generated file in [shape_laws.ml.reference].
   The same file must be produced for byte and native code. *)

let double x = 2 * x

law_? double_grows (x : int) : x > 0 implies_ double x > x
law_? trivial : 1 = 1
