(* TEST
 readonly_files = "link_manifest_a.ml link_manifest_b.ml link_manifest.sh";
 flambda2;
 flags = "-link-jsir";
 {
   setup-ocamlopt.jsir-js-build-env;
   module = "link_manifest_a.ml";
   ocamlopt.jsir-js;
   module = "link_manifest_b.ml";
   ocamlopt.jsir-js;
   module = "link_manifest.ml";
   ocamlopt.jsir-js;
   script = "sh ${test_source_directory}/link_manifest.sh \
     ${test_build_directory} ${ocamlsrcdir} js";
   script;
   program = "${test_build_directory}/link_manifest.js";
   run-js;
   check-program-output;
 }{
   setup-ocamlopt.jsir-wasm-build-env;
   module = "link_manifest_a.ml";
   ocamlopt.jsir-wasm;
   module = "link_manifest_b.ml";
   ocamlopt.jsir-wasm;
   module = "link_manifest.ml";
   ocamlopt.jsir-wasm;
   script = "sh ${test_source_directory}/link_manifest.sh \
     ${test_build_directory} ${ocamlsrcdir} wasm";
   script;
   program = "${test_build_directory}/link_manifest.js";
   run-wasm;
   check-program-output;
 }
*)

(* With -link-jsir, the JSIR files and archives to link can be found through a
   manifest (-I-manifest) that maps them to files with no extension, as in a
   content-addressed store. See link_manifest.sh, which makes such a store of a
   unit and an archive and links this program from it. *)

let () =
  Printf.printf "main: %s %s\n" Link_manifest_a.name Link_manifest_b.name
