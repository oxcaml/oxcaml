(* TEST
 readonly_files = "instantiate_p.mli \
   instantiate_p_impl.ml \
   instantiate_p_impl2.ml \
   instantiate_empty.ml \
   instantiate_aux.ml \
   instantiate_lib.ml";
 flambda2;
 {
   setup-ocamlopt.jsir-js-build-env;
   flags = "-as-parameter";
   module = "instantiate_p.mli";
   ocamlopt.jsir-js;
   flags = "-as-argument-for Instantiate_p";
   module = "instantiate_p_impl.ml";
   ocamlopt.jsir-js;
   flags = "-as-argument-for Instantiate_p";
   module = "instantiate_p_impl2.ml";
   ocamlopt.jsir-js;
   flags = "-parameter Instantiate_p";
   module = "instantiate_empty.ml";
   ocamlopt.jsir-js;
   flags = "-parameter Instantiate_p";
   module = "instantiate_aux.ml";
   ocamlopt.jsir-js;
   flags = "-parameter Instantiate_p";
   module = "instantiate_lib.ml";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl.cmx-js";
   all_modules = "instantiate_empty.cmx-js instantiate_p_impl.cmx-js";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl.cmx-js";
   all_modules = "instantiate_aux.cmx-js instantiate_p_impl.cmx-js";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl.cmx-js";
   all_modules = "instantiate_lib.cmx-js instantiate_p_impl.cmx-js";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl2.cmx-js";
   all_modules = "instantiate_empty.cmx-js instantiate_p_impl2.cmx-js";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl2.cmx-js";
   all_modules = "instantiate_aux.cmx-js instantiate_p_impl2.cmx-js";
   ocamlopt.jsir-js;
   flags = "-instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl2.cmx-js";
   all_modules = "instantiate_lib.cmx-js instantiate_p_impl2.cmx-js";
   ocamlopt.jsir-js;
   flags = "-w -misplaced-attribute";
   module = "instantiate.ml";
   ocamlopt.jsir-js;
   flags = "";
   module = "";
   program = "${test_build_directory}/instantiate.js";
   all_modules = "instantiate_p_impl.cmx-js \
     instantiate_p_impl2.cmx-js \
     instantiate_empty.cmx-js instantiate_aux.cmx-js instantiate_lib.cmx-js \
     instantiate_empty-Instantiate_p_impl.cmx-js \
     instantiate_aux-Instantiate_p_impl.cmx-js \
     instantiate_lib-Instantiate_p_impl.cmx-js \
     instantiate_empty-Instantiate_p_impl2.cmx-js \
     instantiate_aux-Instantiate_p_impl2.cmx-js \
     instantiate_lib-Instantiate_p_impl2.cmx-js \
     instantiate.cmx-js";
   ocamlopt.jsir-js;
   run-js;
   check-program-output;
 }{
   setup-ocamlopt.jsir-wasm-build-env;
   flags = "-as-parameter";
   module = "instantiate_p.mli";
   ocamlopt.jsir-wasm;
   flags = "-as-argument-for Instantiate_p";
   module = "instantiate_p_impl.ml";
   ocamlopt.jsir-wasm;
   flags = "-as-argument-for Instantiate_p";
   module = "instantiate_p_impl2.ml";
   ocamlopt.jsir-wasm;
   flags = "-parameter Instantiate_p";
   module = "instantiate_empty.ml";
   ocamlopt.jsir-wasm;
   flags = "-parameter Instantiate_p";
   module = "instantiate_aux.ml";
   ocamlopt.jsir-wasm;
   flags = "-parameter Instantiate_p";
   module = "instantiate_lib.ml";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_empty.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_aux.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_lib.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_empty.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_aux.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_lib.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-w -misplaced-attribute";
   module = "instantiate.ml";
   ocamlopt.jsir-wasm;
   flags = "-a";
   module = "";
   program = "instantiate_p_impl.cmxa-wasm";
   all_modules = "instantiate_empty-Instantiate_p_impl.cmx-wasm instantiate_aux-Instantiate_p_impl.cmx-wasm instantiate_lib-Instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-a";
   module = "";
   program = "instantiate_p_impl2.cmxa-wasm";
   all_modules = "instantiate_empty-Instantiate_p_impl2.cmx-wasm instantiate_aux-Instantiate_p_impl2.cmx-wasm instantiate_lib-Instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "";
   module = "";
   program = "${test_build_directory}/instantiate.js";
   all_modules = "instantiate_p_impl.cmx-wasm \
     instantiate_p_impl2.cmx-wasm \
     instantiate_empty.cmx-wasm instantiate_aux.cmx-wasm instantiate_lib.cmx-wasm \
     instantiate_p_impl.cmxa-wasm \
     instantiate_p_impl2.cmxa-wasm \
     instantiate.cmx-wasm";
   ocamlopt.jsir-wasm;
   run-wasm;
   check-program-output;
 }{
   setup-ocamlopt.jsir-wasm-build-env;
   flags = "-link-jsir -as-parameter";
   module = "instantiate_p.mli";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -as-argument-for Instantiate_p";
   module = "instantiate_p_impl.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -as-argument-for Instantiate_p";
   module = "instantiate_p_impl2.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -parameter Instantiate_p";
   module = "instantiate_empty.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -parameter Instantiate_p";
   module = "instantiate_aux.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -parameter Instantiate_p";
   module = "instantiate_lib.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_empty.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_aux.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl.cmx-wasm";
   all_modules = "instantiate_lib.cmx-wasm instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_empty-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_empty.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_aux-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_aux.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -instantiate";
   module = "";
   program = "instantiate_lib-Instantiate_p_impl2.cmx-wasm";
   all_modules = "instantiate_lib.cmx-wasm instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -w -misplaced-attribute";
   module = "instantiate.ml";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -a";
   module = "";
   program = "instantiate_p_impl.cmxa-wasm";
   all_modules = "instantiate_empty-Instantiate_p_impl.cmx-wasm instantiate_aux-Instantiate_p_impl.cmx-wasm instantiate_lib-Instantiate_p_impl.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir -a";
   module = "";
   program = "instantiate_p_impl2.cmxa-wasm";
   all_modules = "instantiate_empty-Instantiate_p_impl2.cmx-wasm instantiate_aux-Instantiate_p_impl2.cmx-wasm instantiate_lib-Instantiate_p_impl2.cmx-wasm";
   ocamlopt.jsir-wasm;
   flags = "-link-jsir";
   module = "";
   program = "${test_build_directory}/instantiate.js";
   all_modules = "instantiate_p_impl.cmx-wasm \
     instantiate_p_impl2.cmx-wasm \
     instantiate_empty.cmx-wasm instantiate_aux.cmx-wasm instantiate_lib.cmx-wasm \
     instantiate_p_impl.cmxa-wasm \
     instantiate_p_impl2.cmxa-wasm \
     instantiate.cmx-wasm";
   ocamlopt.jsir-wasm;
   run-wasm;
   check-program-output;
 }
*)

(* Instances of parameterised units, linked from their units and from archives.
   [Instantiate_empty] only has a module type, so the continuation that defines
   its module symbol has no parameter. An instance is a different unit from the
   one it instantiates and from its other instances: each registers its module
   block under its own name. *)

module Lib =
  Instantiate_lib (Instantiate_p) (Instantiate_p_impl)
  [@jane.non_erasable.instances]

module Lib2 =
  Instantiate_lib (Instantiate_p) (Instantiate_p_impl2)
  [@jane.non_erasable.instances]

module One = struct
  let z = 1
end

let () =
  print_endline (Lib.f (module One));
  print_endline (Lib2.f (module One))
