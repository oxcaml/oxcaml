(* TEST
 flambda2;
 {
   setup-ocamlopt.jsir-js-build-env;
   script = "node ${test_source_directory}/stdlib_sourcemap.js \
     ${ocamlsrcdir} ${test_build_directory} js";
   script;
 }{
   setup-ocamlopt.jsir-wasm-build-env;
   script = "node ${test_source_directory}/stdlib_sourcemap.js \
     ${ocamlsrcdir} ${test_build_directory} wasm";
   script;
 }
*)

let () =
  print_int (Stdlib_stable.Int.unsigned_div (Sys.opaque_identity 40) 2)
