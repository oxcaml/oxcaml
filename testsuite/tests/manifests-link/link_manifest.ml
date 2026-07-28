(* TEST
 readonly_files = "helper.ml lib_a.ml lib_b.ml link.sh";
 {
   setup-ocamlopt.byte-build-env;
   module = "helper.ml";
   ocamlopt.byte;
   module = "lib_a.ml";
   ocamlopt.byte;
   module = "lib_b.ml";
   ocamlopt.byte;
   module = "link_manifest.ml";
   flags = "-c";
   ocamlopt.byte;
   script = "sh ${test_source_directory}/link.sh \
     ${test_build_directory} ${ocamlrun} ${ocamlopt_byte} \
     ${ocamlsrcdir}/stdlib ${ocamlsrcdir}/${runtime_dir} \
     cmx";
   output = "${compiler_output}";
   script;
   program = "${test_build_directory}/link_manifest.exe";
   run;
   check-program-output;
 }{
   setup-ocamlc.byte-build-env;
   module = "helper.ml";
   ocamlc.byte;
   module = "lib_a.ml";
   ocamlc.byte;
   module = "lib_b.ml";
   ocamlc.byte;
   module = "link_manifest.ml";
   flags = "-c";
   ocamlc.byte;
   script = "sh ${test_source_directory}/link.sh \
     ${test_build_directory} ${ocamlrun} ${ocamlc_byte} \
     ${ocamlsrcdir}/stdlib ${ocamlsrcdir}/${runtime_dir} \
     cmo -use-runtime ${ocamlrun}";
   output = "${compiler_output}";
   script;
   program = "${test_build_directory}/link_manifest.exe";
   run;
   check-program-output;
 }
*)

(* Test that link inputs given by their bare names ('foo.cmx', 'lib.cmxa') are
   resolved through manifest files (-I-manifest), including when the manifest
   maps them to content-addressed paths that do not retain the extension. In
   that case the companion object files ('foo.o', 'lib.a') must be listed as
   separate manifest entries and are resolved through the load path too.

   See link.sh for the details: it moves all compilation artifacts into a
   simulated content-addressed store (extensionless blob names), writes a
   manifest, and links the program by bare names only. It also checks that a
   missing '.a' entry is an error for a non-empty archive, and is accepted for
   an empty archive. *)

let () =
  print_endline (Helper.greeting ());
  Printf.printf "archive units: %s %s\n" Lib_a.name Lib_b.name
