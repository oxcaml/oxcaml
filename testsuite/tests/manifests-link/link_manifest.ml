(* TEST
 readonly_files = "helper.ml link.sh";
 {
   setup-ocamlopt.byte-build-env;
   module = "helper.ml";
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

let () = print_endline (Helper.greeting ())
